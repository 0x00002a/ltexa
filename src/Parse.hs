-- Copyright (C) 2020 Natasha England-Elbro
--
-- This file is part of ltexa.
--
-- ltexa is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- ltexa is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with ltexa.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Parse where

import Control.Monad (void, when, (>=>))
import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as LNE
import Text.Megaparsec.Debug (dbg)
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Text.Read (readEither)
import Data.Stack
import Data.Text (Text, append, find, isInfixOf, pack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Void
import qualified Debug.Trace
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as PT
import Text.Megaparsec.Char
import Types

type Parser = PT.Parsec Text Text

type ParseError = PT.ParseErrorBundle Text Text

parse :: StreamT -> Either Text [ParseMessage]
parse txt =
  handleResult $
    firstPass txt >>= PT.runParser (parseLtexOutput freshState) "src"
  where
    handleResult :: Either ParseError PState -> Either Text [ParseMessage]
    handleResult parser = case parser of
      Left err -> Left $ handleErrs err
      Right xs -> Right $ messages_ xs

    handleErrs :: ParseError -> Text
    handleErrs e = pack $ PT.errorBundlePretty e

instance PT.ShowErrorComponent Text where
  showErrorComponent txt = T.unpack txt

-- |
-- Takes "raw" initial tex input and converts it to the internally expected form
--
-- Currently only unwraps wrapped lines.
firstPass :: Text -> Either ParseError Text
firstPass txt = T.concat <$> PT.parse doParse "" txt
  where
    doParse = PT.manyTill wrappedLine PT.eof

class TypedMessage a where
  getMsgType :: a -> MessageType

instance TypedMessage AppMessage where
  getMsgType = app_msg_type_

instance TypedMessage ParseMessageData where
  getMsgType = msg_type_

instance TypedMessage ParseMessage where
  getMsgType (AppMsg msg) = getMsgType msg
  getMsgType (Msg msg) = getMsgType msg
  getMsgType RerunDetected = InfoMsg

data PState = PState
  { curr_page_ :: Int,
    files_ :: Stack FilePath,
    messages_ :: [ParseMessage],
    at_eof_ :: Bool
  }

reportMsg st body num tp =
  reportWithStackTrace
    st
    body
    num
    tp
    Nothing

reportWithStackTrace st body num tp strace = buildMsg
  where
    buildMsg =
      ParseMessageData
        body
        num
        tp
        (curr_page_ st)
        ((stackPeek . files_) st)
        strace
        (Just ["LaTeX"])

freshState = PState {curr_page_ = 0, files_ = stackNew, messages_ = [], at_eof_ = False}


foldMany :: Monad f => (a -> f a) -> a -> f a
foldMany func = func >=> foldMany func

parseLtexOutput :: PState -> Parser PState
parseLtexOutput st = maybeParseInner st --(fileStart st >>= maybeParseInner >>= fileEnd) <* PT.eof
    where
        maybeParseInner st = (st <$ PT.try (text "\n")) <|> ltexParsers st

ltexParsers :: PState -> Parser PState
ltexParsers st = parseError st --PT.choice (base_parsers ++ [st <$ consumeNoise])
  where
    base_parsers = PT.try `map` expr_parsers
    expr_parsers =
      [ parseError st ]
        --Just <$> runawayArgument st,
        {- Just <$> badBox st,
        Just <$> latexWarning st,
        Just <$> missingInclude st,
        generalNoise,
        pageEnd st,
        genericMsg st,
        providesMsg st,
        fileEnd st,
        fileStart st,
        (\_ -> Just st {at_eof_ = True}) <$> PT.eof
      ] -}

--consumeNoise = PT.anySingle

consumeLine :: Parser Text
consumeLine = pack <$> PT.manyTill anyChar eol

-- |
-- Unwraps a wrapped line. Not perfect due since it may "unwrap" a line which
-- was never wrapped in the first place, but there is no way around that.
wrappedLine = loopOnLine ""
  where
    loopOnLine txt =
      PT.manyTill PT.anySingle (PT.choice [PT.eof, void $ PT.try $ PT.lookAhead newline])
        >>= \chars ->
          PT.getSourcePos
            >>= \pos ->
              PT.choice
                [ (\_ -> txt <> pack chars) <$> PT.eof,
                  (newline :: Parser Char)
                    >> if PT.unPos (PT.sourceColumn pos) >= 80
                      then loopOnLine $ txt `append` pack chars
                      else return $ txt `append` pack chars `append` "\n"
                ]

word :: Parser String
word = PT.some letterChar

-- |
--    Eats the input up until the first file start.
--    Prevents weirdness from trying to parse the introduction
--    which varies between implementations
upToFirstFile =
  PT.manyTill consumeLine fStart
    >> makeMsg <$> PT.getSourcePos
  where
    makeMsg pos =
      AppMsg $
        AppMessage "Consumed introduction" pos DebugMsg
    fStart =
      PT.try $
        PT.lookAhead $
          char '('
            >> PT.manyTill PT.anySingle (PT.try newline <|> char ')')

oneOfStr xs = PT.choice $ string `map` xs

manyTillLH p sep =
  PT.manyTill p (PT.lookAhead sep)
    <> sep

-- |
-- Error message logged by the TeX engine. In the form:
-- ! <Message>
-- [stack trace]
-- l. <line number of error> <Additional context
--                                             with error column>
--
-- where
--     - stack trace is a list newline separated <source>
--     - Message may be wrapped
--     - Additional context may print for example the \command which caused the error, and will print the character on which it occurred on the next line indented to the character before (as shown in the format above)
parseError :: PState -> Parser PState
parseError st = do
    char '!'
    actualSpace
    body <- dbg "He2" $ pack <$> PT.manyTill PT.anySingle eol
    (ctxs, line) <- dbg "He" $ parseContextLines body
    return $
            addMsg st . Msg $
              reportWithStackTrace st body (read <$> line) ErrMsg (Just ctxs)
  where
    parseContextLines body = do
         (err_ctx, line) <- parseMessages
         msg <- dbg "He3" $ handleFatal body
         return (handlePossibleFatal err_ctx msg, line)


    parseMessages = dbg "he4" $
      PT.choice messageBodies
          >>= \btrace ->
            makeContext btrace
              <$> PT.optional lineCtx

    --msgLine = parseMessage <|> lineCtxStart
    messageBodies = [PT.many (parseMessage <|> (blankLine >> return ""))]

    makeContext :: [Text] -> Maybe (Maybe ErrorLocation, String) -> (ErrorContext, Maybe String)
    makeContext btrace location = case location of
      Just (loc, line) -> makeC loc (Just line)
      Nothing -> makeC Nothing Nothing
      where
        makeC loc line =
          (ErrorContext (btrace) loc, line)

    parseMessage :: Parser Text
    parseMessage =
      PT.choice [PT.try anglesCtx, PT.try elidedCtx]

    lineCtxStart = string "l."

    elidedCtx :: Parser Text
    elidedCtx =
      PT.choice
        [ PT.try $
            string "\\"
              <> (pack <$> PT.many letterChar)
              <> (pack <$> PT.many letterChar)
              <> (string "->" <|> string "..."),
          pack <$> (PT.try $ PT.manyTill PT.anySingle $ string "...")
        ]

    anglesCtx :: Parser Text
    anglesCtx =
      string "<"
        <> (PT.try readStar <|> PT.try ltxOrSpace <|> string "*")
        <> (pack <$> PT.manyTill PT.anySingle eol)
        >>= \msg -> return msg
    readStar :: Parser Text
    readStar = string "read " <> (pack <$> PT.many (PT.noneOf (" >" :: [Char])))
    ltxOrSpace = pack <$> PT.some (lowerChar <|> actualSpace)
    lineCtx =
      string "l."
        >> ( PT.some digitChar
               <* char ' '
           )
        >>= \line ->
          PT.manyTill PT.anySingle eol
            >>= \before_err ->
              PT.skipMany actualSpace >> PT.manyTill PT.anySingle eol
                >>= \after_err -> case before_err ++ after_err of
                  [] -> return (Nothing, line)
                  _ ->
                    return (Just $ ErrorLocation (pack before_err) (pack after_err), line)

    {- LaTeX likes to print lines of whitespace seemingly randomly amongst the
        error output, this skips them -}
    blankLine :: Parser ()
    blankLine = space
    sortMsgs (ctxs, line) = (pack `map` catMaybes ctxs, line)
    splitMsgs :: [(Maybe String, Maybe String)] -> ([Maybe String], String)
    splitMsgs msgs = doSplit $ splitTupleList msgs
      where
        doSplit (msgs, lines) =
          (msgs, (!! 0) <$> catMaybes lines)
    handlePossibleFatal err_ctx Nothing = err_ctx
    handlePossibleFatal err_ctx (Just msg) =
      err_ctx {stack_trace_ = stack_trace_ err_ctx ++ [pack msg]}
    secondSplit (msgs, []) = (msgs, Nothing)
    secondSplit (msgs, line) = (msgs, Just line)
    checkFatal "Emergency Stop." = True
    checkFatal _ = False
    handleFatal msg =
      if checkFatal msg
        then
          PT.optional begStars
            >> PT.manyTill PT.anySingle eol
            >>= \ctx -> return $ Just $ ": " ++ ctx
        else return Nothing
      where
        begStars =
          PT.count 3 (char '*')
            >> actualSpace

-- |
-- Bad box warning. Too complex a match to write in full here
-- (regex version is a mess). Read the code
badBox :: PState -> Parser PState
badBox st = do_match >>= process
  where
    do_match :: Parser Text
    do_match =
      oneOfStr box_choices
        <> string " "
        <> box_match
        <> string " "
        <> string "("
    box_choices :: [Text]
    box_choices = ["Overfull", "Underfull", "Loose", "Tight"]
    box_match = string "\\" <> (T.singleton <$> PT.oneOf ("hv" :: [Char])) <> string "box"
    process :: Text -> Parser PState
    process curr_msg =
      PT.choice
        [ PT.try normalMsgDesc,
          detected_message
        ]
        >>= \(line, msg) ->
          when ("hbox" `isInfixOf` (msg <> curr_msg)) filterOffendingTxt
            >> (return $ (addMsg st . Msg) $ reportMsg st (curr_msg <> msg) line WarnMsg)

    filterOffendingTxt =
      newline
        >> consumeLine
        >> return ()
    normalMsgDesc :: Parser (Maybe Int, Text)
    normalMsgDesc = do 
        msg1 <- T.pack <$> PT.manyTill (PT.noneOf ("\n" :: String)) (PT.try $ PT.lookAhead descChoices)
        (line, msg_second) <- descChoices
        return (line, msg1 <> msg_second)

    descChoices :: Parser (Maybe Int, Text)
    descChoices =
      PT.choice
        [ PT.try paraChoice,
          PT.try detectedChoice
        ]

    paraChoice = do
        msg <- string " in " <> oneOfStr ["paragraph", "alignment"] <* string " at lines "
        m1 <- PT.manyTill digitChar (string "--")
        m2 <- PT.manyTill digitChar (PT.try $ PT.notFollowedBy digitChar)
        return (Just (min (read m1) (read m2)), msg)

    detectedChoice =
      consumeLine
        >>= \line ->
          if " detected at line " `isInfixOf` line
            then
              PT.manyTill digitChar (PT.try $ PT.notFollowedBy digitChar)
                >>= \m1 -> return (Just $ read m1, line <> pack m1)
            else PT.customFailure "Line does not contained detected message"

    detected_message =
      PT.manyTill PT.anySingle (PT.try $ string " while \\output is active")
        >>= \msg -> return (Nothing, pack msg)

toByteString :: String -> B.ByteString
toByteString = encodeUtf8 . pack

anyExceptNl = PT.satisfy pred
  where
    pred '\n' = False
    pred _ = True

-- |
-- Warning in the form
-- LaTeX Warning: <Message>
-- <blankline>
--
-- Note that Message may be wrapped but even so there will still be a newline after
latexWarning :: PState -> Parser PState
latexWarning st = do
    main_provider <- chChoices
    second_provider <- PT.manyTill anyExceptNl ( PT.try $ string "Warning: " )
    msg <- upToBlankline
    return $ retrMsg [main_provider, pack second_provider] $ tryFindLine msg

  where
    upToBlankline = pack <$> PT.manyTill PT.anySingle (PT.try $ string "\n\n")

    tryFindLine txt = case PT.parse findLine "" txt of
      Left _ -> Nothing
      Right r -> Just r
      where
        findLine = do 
            msg <- PT.manyTill PT.anySingle (PT.try $ PT.choice [lineNumPrefix, PT.eof >> return ""])
            num <- PT.optional (PT.some digitChar)

            return (pack msg, read <$> num)
        lineNumPrefix :: Parser Text
        lineNumPrefix = string " on input line "

    retrMsg _ Nothing = undefined
    retrMsg providers (Just (body, line)) =
      addMsg st . Msg $ addProviders (reportMsg st body line WarnMsg) providers
      where
        addProviders :: ParseMessageData -> [Text] -> ParseMessageData
        addProviders p msgs =
          p
            { providers_ =
                providers_ p
                  >>= \exi -> Just $ exi ++ msgs
            }
    chChoices =
      oneOfStr ["LaTeX", "Package", "Class", "pdfTeX"]
        >> string " "
-- |
-- Start of a new file is in the form:
-- (<filepath>[contents])
--
-- notes:
--     - contents may contain newlines
fileStart :: PState -> Parser PState
fileStart st = doParse >>= updateState
  where
    doParse = PT.try (PT.optional newline) >> parseFName -- Either newline (name \n rest ) OR (name)
    parseFName =
      char '('
        >> PT.many matched
        >>= \case "" -> PT.unexpected PT.EndOfInput; val -> return val
    matched = PT.satisfy notIgnored
    ignoredSet = "(){} \n"
    notIgnored str = case find (str ==) ignoredSet of
      Just _ -> False
      Nothing -> True

    updateState fname =
      PT.getSourcePos
        >>= \pos ->
          return $
              addMsg (st {files_ = stackPush (files_ st) fname}) $
                AppMsg $
                  AppMessage ("Pushed: " `append` pack fname) pos TraceMsg

{-
End of file is simply a ')' character. As far as I can tell there are no
special rules about where it may appear or what surrounds it.
-}
fileEnd :: PState -> Parser PState
fileEnd st = doParse >> updateState
  where
    doParse =
      char ')'
    updateState =
      generateMsg (files_ st)
    generateMsg :: Stack FilePath -> Parser PState
    generateMsg files =
      PT.getSourcePos >>= \pos ->
        if stackIsEmpty files
          then return $ errReport pos
          else
            if stackSize files == 1
              then consumeLatexmkNoise `uncurry` popFile st pos
              else return $ fst $ popFile st pos
    errReport :: PT.SourcePos -> PState
    errReport pos = addMsg st $ AppMsg $ AppMessage "Extra ) in log" pos WarnMsg
    -- nextRun :: Stack FilePath -> Parser PState
    --nextRun files = case stackPeek files of
    --Just file -> consumeLatexmkNoise file

    consumeLatexmkNoise :: PState -> String -> Parser PState
    consumeLatexmkNoise st root =
      PT.manyTill
        PT.anySingle
        ( PT.try $
            PT.lookAhead $
              char '('
                >> string (T.pack root)
        )
        >> PT.getSourcePos
        >>= \pos ->
          return $
            addMsgs
              st
              [ AppMsg $ AppMessage "Rerun detected" pos DebugMsg,
                RerunDetected
              ]

    logPop :: PT.SourcePos -> String -> ParseMessage
    logPop pos file = AppMsg $ AppMessage ("Popped: " `append` pack file) pos TraceMsg

    popFile :: PState -> PT.SourcePos -> (PState, String)
    popFile st pos =
      case stackPop (files_ st) of
        Just (files, file) -> (addMsg (st {files_ = files}) $ logPop pos file, file)

addMsg :: PState -> ParseMessage -> PState
addMsg st msg = addMsgs st [msg]

addMsgs :: PState -> [ParseMessage] -> PState
addMsgs st msg = st {messages_ = messages_ st ++ msg}

pageEnd :: PState -> Parser (Maybe PState)
pageEnd st =
  do_parse
    >>= \pg ->
      PT.getSourcePos
        >>= \pos ->
          return $
            Just $
              addMsg (updateState pg) $ AppMsg $ AppMessage (pack $ "Beginning page: " <> show pg) pos TraceMsg
  where
    do_parse = char '[' >> PT.some digitChar <* char ']'
    updateState :: String -> PState
    updateState new_pg = st {curr_page_ = read new_pg + 1}

genericMsg _ = doParse
  where
    doParse =
      oneOfStr ["Package", "Document", "LaTeX"]
        >> chChoice
        >> string "info: "
        >> return Nothing
    chChoice = PT.manyTill word actualSpace

providesMsg :: PState -> Parser (Maybe PState)
providesMsg st = doParse
  where
    doParse =
      oneOfStr ["Document Class", "File", "Package"]
        >> string ": "
        >> consumeLine
        >> ( PT.getSourcePos >>= \pos ->
               return $
                 Just $
                   addMsg st $
                     AppMsg $ AppMessage "Found provides" pos TraceMsg
           )

generalNoise = doParse >> return Nothing
  where
    doParse =
      char '\\'
        >> word
        >> char '\\'
        >> PT.many lowerChar
        >> PT.some digitChar
        >> newline

{-
Printed when a runaway argument is detected, provides some tokens as context
and then calls print_err.
Format:
Runaway ...\n<list of tokens>\n! <Error message>

-}
runawayArgument :: PState -> Parser PState
runawayArgument st = do
    before <- runawaySeg
    PT.manyTill consumeLine (PT.try $ PT.lookAhead $ string "! ")
    body <- PT.manyTill consumeLine (PT.try $ lineIdent)
    return $ addMsg st (Msg $ genMsg before (foldr append "" body))
  where
    runawaySeg :: Parser Text
    runawaySeg =
      string "Runaway "
        >> consumeLine
        >> string "{"
        <> (pack <$> PT.many letterChar)

    genMsg before msg =
      reportWithStackTrace
        st
        ("Runaway argument: " <> msg)
        Nothing
        ErrMsg
        ( Just $
            ErrorContext
              []
              (Just $ ErrorLocation before "")
        )

transformErr :: Either String a -> Parser a
transformErr (Left err) = fail err
transformErr (Right v) = return v

lineIdent :: Parser Int
lineIdent = (readEither . (:[]) <$> (char' 'l' >> char' '.' >> digitChar)) >>= transformErr

anyChar :: (PT.MonadParsec e s m, PT.Token s ~ Char) => m Char
anyChar = PT.anySingle

{-
Missing file for \include is not reported as an error (unlike with \input{}).
-}
missingInclude :: PState -> Parser PState
missingInclude st =
  string "No file "
    >> PT.manyTill PT.anySingle (PT.try $ string ".tex")
      <* char '.'
      <* eol
    >>= \missing_fp ->
      return $
        addMsg st . Msg $
          reportMsg
            st
            ("Missing include file: " <> pack missing_fp <> ".tex")
            Nothing
            WarnMsg

text :: Text -> Parser Text
text = PT.chunk

splitTupleList :: [(a, b)] -> ([a], [b])
splitTupleList = foldr doSep ([], [])
  where
    doSep (ta, tb) (la, ba) = (ta : la, tb : ba)

actualSpace :: Parser Char
actualSpace = char ' '
