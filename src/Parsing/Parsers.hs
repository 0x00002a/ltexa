{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Parsing.Parsers where

import Debug.Trace (trace)
import Control.Monad (void, when, (>=>))
import Text.Megaparsec.Debug (dbg)
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Text (Text, append, find, isInfixOf, pack, unpack)
import qualified Data.Text as T
import Text.Megaparsec ((<|>), try)
import qualified Text.Megaparsec as PT
import Text.Megaparsec.Char
import Data.Stack (Stack, stackPop, stackIsEmpty, stackPush)
import Text.Read (readEither)
import Types

import Parsing.ParseUtil
import Parsing.PState
import Prelude hiding (lines)

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
    body <- parseBody
    (ctxs, line) <- parseContextLines body
    return $
            addMsg st . Msg $
              reportWithStackTrace st body (read <$> line) ErrMsg (Just ctxs)
  where
    parseContextLines body = do
         (err_ctx, line) <- parseMessages
         msg <- handleFatal body
         return (handlePossibleFatal err_ctx msg, line)

    parseBody =
        consumeLine
        where
            bodyEnd = PT.lookAhead $ try $ PT.notFollowedBy (try (text "l.") <|> T.singleton <$> letterChar)
            --latexErr = text "LaTeX Error: " ><> consmeLine ><> PT.manyTill anyChar bodyEnd
        -- ><> ((foldl (<>) "" <$> (lines ><> PT.manyTill consumeLine bodyEnd)))

    parseMessages :: Parser (ErrorContext, Maybe String)
    parseMessages = do
      btrace <- PT.choice messageBodies
      line <- PT.optional $ PT.skipManyTill anyChar lineCtx
      return $ makeContext btrace line

    --msgLine = parseMessage <|> lineCtxStart
    messageBodies = [PT.manyTill (try parseMessage <|> (blankLine >> return "")) (("" <$ try $ PT.lookAhead lineCtx) <|> text " ")]

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

    --lineCtxStart = string "l."

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
    lineCtx = do
      line <- string "l." >> (PT.some digitChar <* char ' ')
      before_err <- PT.manyTill PT.anySingle eol
      after_err <- try $ PT.optional $ PT.some " " >> PT.manyTill PT.anySingle eol
      case before_err ++ (fromMaybe [] after_err) of
        [] -> return (Nothing, line)
        _ -> return (Just $ ErrorLocation (pack before_err) (fromMaybe "" (pack <$> after_err)), line)

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
            else (PT.getSourcePos
                >>= \pos -> PT.customFailure
                    [AppMsg $ AppMessage "Line does not contained detected message" pos WarnMsg])

    detected_message =
      PT.manyTill PT.anySingle (PT.try $ string " while \\output is active")
        >>= \msg -> return (Nothing, pack msg)

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
-- (<filepath>[\n contents])
--
-- notes:
--     - file path starts with either ./ or /
fileStart :: PState -> Parser PState
fileStart st = doParse >>= updateState
  where
    doParse = char '(' >> parseFName -- Either newline (name \n rest ) OR (name)
    pathStart :: Parser Text
    pathStart = optionally' (text) (\s -> (s <>) <$> text "/") "."
    parseFName =
        pathStart ><> (pack <$> PT.manyTill anyChar endOfName)
    endOfName = (try newline) <|> (PT.lookAhead $ char ')')

    updateState fname =
      PT.getSourcePos
        >>= \pos ->
          return $
              addMsg (st {files_ = stackPush (files_ st) (unpack fname)}) $
                AppMsg $
                  AppMessage ("Pushed: " `append` fname) pos TraceMsg

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
          else return $ fst $ popFile st pos
            {-if stackSize files == 1
              then consumeLatexmkNoise `uncurry` popFile st pos
              else return $ fst $ popFile st pos-}
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

pageEnd :: PState -> Parser PState
pageEnd st =
  do_parse
    >>= \pg ->
      PT.getSourcePos
        >>= \pos ->
          return $
              addMsg (updateState pg) $ AppMsg $ AppMessage (pack $ "Beginning page: " <> show pg) pos TraceMsg
  where
    do_parse = char '[' >> PT.some digitChar <* char ']'
    updateState :: String -> PState
    updateState new_pg = st {curr_page_ = read new_pg + 1}

genericMsg st = st <$ doParse
  where
    doParse =
      oneOfStr ["Package", "Document", "LaTeX"]
        >> chChoice
        >> string "info: "
    chChoice = PT.manyTill word actualSpace

providesMsg :: PState -> Parser PState
providesMsg st = doParse
  where
    doParse =
      oneOfStr ["Document Class", "File", "Package"]
        >> string ": "
        >> consumeLine_
        >> ( PT.getSourcePos >>= \pos ->
               return $
                   addMsg st $
                     AppMsg $ AppMessage "Found provides" pos TraceMsg
           )

generalNoise :: PState -> Parser PState
generalNoise st = PT.choice parsers >>= writeMsg
  where
    parsers = map PT.try [txtNoise, T.singleton <$> doParse, bracketNoise]
    txtNoise =
        (T.singleton <$> letterChar)
        ><> consumeLine
    writeMsg noise =
            PT.getSourcePos
            >>= \pos -> return $ addMsg st $ AppMsg $ AppMessage ("consumed noise: " <> noise) pos DebugMsg

    bracketNoise = text "[" ><> (foldl (<>) "" <$> PT.manyTill (T.singleton <$> anyChar) (text "]"))

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
              (Just $ ErrorLocation before ""))

lineIdent :: Parser Int
lineIdent = (readEither . (:[]) <$> (char' 'l' >> char' '.' >> digitChar)) >>= transformErr

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

{-
   LaTeX2e ...
    L3 ...

    Basically match LaTeX2e and then consume that line and the next
-}
latexInfoIntro :: PState -> Parser PState
latexInfoIntro st = appendMsg <$ (text "LaTeX2e" >> consumeLine_ >> consumeLine_)
    where
        appendMsg = addMsg st . Msg $ reportMsg st ("Consumed LaTeX2e intro") Nothing DebugMsg


