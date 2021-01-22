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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Monad (when)
import qualified Data.ByteString as B
import Data.Maybe (catMaybes)
import Data.Stack
import Data.Text (Text, append, find, isInfixOf, pack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Debug.Trace
import Text.Parsec ((<|>))
import qualified Text.Parsec as PT
import Text.Parsec.Char
import Types

type Parser = PT.Parsec Text PState

parse :: StreamT -> Either Text [ParseMessage]
parse txt =
  handleResult $
    firstPass txt >>= PT.runParser parseLtexOutput freshState "src"
  where
    handleResult parser = case parser of
      Left err -> Left $ pack $ show err
      Right xs -> Right $ catMaybes xs

-- |
-- Takes "raw" initial tex input and converts it to the internally expected form
--
-- Currently only unwraps wrapped lines.
firstPass :: Text -> Either PT.ParseError Text
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

messages :: ParseMessage -> [ParseMessage]
messages (MultiMessage msgs) = concatMap messages msgs
messages msg = [msg]

data PState = PState
  { curr_page_ :: Integer,
    files_ :: Stack FilePath
  }

reportMsg body num tp =
  reportWithStackTrace
    body
    num
    tp
    Nothing

reportWithStackTrace body num tp strace = build <$> PT.getState
  where
    build st =
      ParseMessageData
        (pack body)
        (read <$> num)
        tp
        (curr_page_ st)
        ((stackPeek . files_) st)
        strace
        (Just ["LaTeX"])

freshState = PState {curr_page_ = 0, files_ = stackNew}

parseLtexOutput =
  ((: []) . Just <$> upToFirstFile)
    <> PT.manyTill ltexParsers PT.eof

ltexParsers = PT.choice (base_parsers ++ [consumeNoise])
  where
    base_parsers = PT.try `map` expr_parsers
    expr_parsers =
      [ Just <$> parseError,
        Just <$> runawayArgument,
        Just <$> badBox,
        Just <$> latexWarning,
        Just <$> missingInclude,
        generalNoise,
        pageEnd,
        genericMsg,
        providesMsg,
        fileEnd,
        fileStart
      ]

consumeNoise = anyChar >> return Nothing

consumeLine = PT.manyTill anyChar PT.endOfLine

-- |
-- Unwraps a wrapped line. Not perfect due since it may "unwrap" a line which
-- was never wrapped in the first place, but there is no way around that.
wrappedLine = loopOnLine ""
  where
    loopOnLine txt =
      PT.manyTill anyChar (PT.try $ PT.lookAhead newline)
        >>= \chars ->
          PT.getPosition
            >>= \pos ->
              newline
                >> if PT.sourceColumn pos >= 80
                  then loopOnLine $ txt `append` pack chars
                  else return $ txt `append` pack chars `append` "\n"

word :: Parser String
word = PT.many1 letter

-- |
--    Eats the input up until the first file start.
--    Prevents weirdness from trying to parse the introduction
--    which varies between implementations
upToFirstFile =
  PT.manyTill consumeLine fStart
    >> makeMsg <$> PT.getPosition
  where
    makeMsg pos =
      AppMsg $
        AppMessage "Consumed introduction" pos DebugMsg
    fStart =
      PT.try $
        PT.lookAhead $
          char '('
            >> PT.manyTill anyChar (PT.try newline <|> char ')')

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
parseError :: Parser ParseMessage
parseError =
  char '!'
    >> actualSpace
    >> PT.manyTill anyChar PT.endOfLine
    >>= \body ->
      parseContextLines body
        >>= \(ctxs, line) ->
          Msg
            <$> reportWithStackTrace body line ErrMsg (Just ctxs)
  where
    parseMessages =
      PT.manyTill
        anyChar
        ( PT.try $
            PT.lookAhead $
              parseMessage
                <|> lineCtxStart
                <|> (char '!' >> return Nothing)
        )
        >> PT.choice
          [ PT.try $
              PT.many
                (parseMessage <|> blankLine),
            PT.lookAhead $
              char '!'
                >> PT.skipMany1 PT.anyChar
                >> return [Nothing]
          ]
          >>= \btrace ->
            makeContext btrace
              <$> PT.optionMaybe lineCtx

    makeContext :: [Maybe String] -> Maybe (Maybe ErrorLocation, String) -> (ErrorContext, Maybe String)
    makeContext btrace location = case location of
      Just (loc, line) -> makeC (loc) (Just line)
      Nothing -> makeC Nothing Nothing
      where
        makeC loc line =
          (ErrorContext (map pack $ catMaybes btrace) loc, line)

    parseMessage :: Parser (Maybe String)
    parseMessage =
      PT.choice [PT.try anglesCtx, PT.try elidedCtx]

    lineCtxStart =
      string "l."
        >> return Nothing

    elidedCtx =
      PT.choice
        [ PT.try $
            string "\\"
              <> PT.many letter
              <> PT.many letter
              <> (string "->" <|> string "..."),
          PT.try $ PT.manyTill anyChar $ string "..."
        ]
        >> return Nothing

    anglesCtx :: Parser (Maybe String)
    anglesCtx =
      string "<"
        <> (PT.try readStar <|> PT.try ltxOrSpace <|> string "*")
        <> PT.manyTill anyChar PT.endOfLine
        >>= \msg -> return $ Just msg
    readStar = string "read " <> PT.many (noneOf " >")
    ltxOrSpace = PT.many1 (lower <|> actualSpace)
    lineCtx =
      string "l."
        >> ( PT.many1 digit
               <* char ' '
           )
        >>= \line ->
          PT.manyTill anyChar PT.endOfLine
            >>= \before_err ->
              PT.skipMany actualSpace >> PT.manyTill anyChar PT.endOfLine
                >>= \after_err -> case before_err ++ after_err of
                  [] -> return (Nothing, line)
                  _ ->
                    return (Just $ ErrorLocation (pack before_err) (pack after_err), line)

    {- LaTeX likes to print lines of whitespace seemingly randomly amongst the
        error output, this skips them -}
    blankLine :: Parser (Maybe String)
    blankLine =
      PT.manyTill actualSpace PT.endOfLine
        >> return Nothing
    sortMsgs (ctxs, line) = (pack `map` catMaybes ctxs, line)
    splitMsgs :: [(Maybe String, Maybe String)] -> ([Maybe String], String)
    splitMsgs msgs = doSplit $ splitTupleList msgs
      where
        doSplit (msgs, lines) =
          (msgs, (!! 0) <$> catMaybes lines)
    parseContextLines body =
      parseMessages
        >>= \(err_ctx, line) ->
          handleFatal body
            >>= \msg -> return (handlePossibleFatal err_ctx msg, line)

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
            >> PT.manyTill anyChar PT.endOfLine
            >>= \ctx -> return $ Just $ ": " ++ ctx
        else return Nothing
      where
        begStars =
          PT.count 3 (char '*')
            >> actualSpace

-- |
-- Bad box warning. Too complex a match to write in full here
-- (regex version is a mess). Read the code
badBox = do_match >>= process
  where
    do_match =
      oneOfStr box_choices
        <> string " "
        <> box_match
        <> string " "
        <> string "("
    box_choices = ["Overfull", "Underfull", "Loose", "Tight"]
    box_match = string "\\" <> ((: []) <$> PT.oneOf "hv") <> string "box"
    process curr_msg =
      PT.choice
        [ PT.try normalMsgDesc,
          detected_message
        ]
        >>= \(line, msg) ->
          when ("hbox" `isInfixOf` pack (msg ++ curr_msg)) filterOffendingTxt
            >>= (\_ -> Msg <$> reportMsg (curr_msg ++ msg) line WarnMsg)

    filterOffendingTxt =
      newline
        >> consumeLine
        >> return ()
    normalMsgDesc =
      PT.manyTill (noneOf "\n") (PT.try $ PT.lookAhead descChoices)
        >>= \msg1 ->
          descChoices
            >>= \(line, msg_second) -> return (line, msg1 ++ msg_second)
    descChoices =
      PT.choice
        [ PT.try paraChoice,
          PT.try detectedChoice
        ]
    paraChoice =
      string " in "
        <> oneOfStr ["paragraph", "alignment"]
        <* string " at lines "
        >>= \msg ->
          PT.manyTill digit (string "--")
            >>= \m1 ->
              PT.manyTill digit (PT.try $ PT.notFollowedBy digit)
                >>= \m2 -> return (Just (min m1 m2), msg)
    detectedChoice =
      consumeLine
        >>= \line ->
          if " detected at line " `isInfixOf` pack line
            then
              PT.manyTill digit (PT.try $ PT.notFollowedBy digit)
                >>= \m1 -> return (Just m1, line ++ m1)
            else PT.unexpected "Line does not contained detected message"

    detected_message =
      PT.manyTill anyChar (PT.try $ string " while \\output is active")
        >>= \msg -> return (Nothing, msg)

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
latexWarning =
  chChoices
    >>= \main_provider ->
      PT.manyTill
        anyExceptNl
        ( PT.try $
            string "Warning: "
        )
        >>= \second_provider ->
          upToBlankline
            >>= \msg ->
              retrMsg [main_provider, second_provider] $ tryFindLine msg
  where
    upToBlankline = PT.manyTill anyChar (PT.try $ string "\n\n")

    tryFindLine txt = case PT.parse findLine "" txt of
      Left _ -> Nothing
      Right r -> Just r
      where
        findLine =
          PT.manyTill
            anyChar
            (PT.try $ PT.choice [lineNumPrefix, PT.eof >> return ""])
            >>= \msg ->
              PT.optionMaybe (PT.many1 digit)
                >>= \num -> return (msg, num)
        lineNumPrefix = string " on input line "

    retrMsg providers (Just (body, line)) =
      Msg . (`addProviders` providers)
        <$> reportMsg body line WarnMsg
      where
        addProviders :: ParseMessageData -> [String] -> ParseMessageData
        addProviders p msgs =
          p
            { providers_ =
                providers_ p
                  >>= \exi -> Just . (exi ++) $ map pack msgs
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
fileStart = doParse >>= updateState
  where
    doParse = PT.try (PT.optional newline) >> parseFName -- Either newline (name \n rest ) OR (name)
    parseFName =
      char '('
        >> PT.many matched
        >>= \case "" -> PT.unexpected "Err"; val -> return val
    matched = satisfy notIgnored
    ignoredSet = "(){} \n"
    notIgnored str = case find (str ==) ignoredSet of
      Just _ -> False
      Nothing -> True

    updateState fname =
      PT.getPosition
        >>= \pos ->
          PT.modifyState
            ( \st -> st {files_ = stackPush (files_ st) fname}
            )
            >> return
              ( Just . AppMsg $
                  AppMessage ("Pushed: " `append` pack fname) pos TraceMsg
              )

{-
End of file is simply a ')' character. As far as I can tell there are no
special rules about where it may appear or what surrounds it.
-}
fileEnd = doParse >> updateState
  where
    doParse =
      char ')'
    updateState =
      PT.getState
        >>= \st ->
          Just <$> generateMsg (files_ st) st
    generateMsg files st =
      PT.getPosition >>= \pos ->
        if stackIsEmpty files
          then return $ errReport pos
          else
            if stackSize files == 1
              then nextRun files <* popFile st
              else logPop pos <$> popFile st
    errReport :: PT.SourcePos -> ParseMessage
    errReport pos = AppMsg $ AppMessage "Extra ) in log" pos WarnMsg
    nextRun files = case stackPeek files of
      Just file -> consumeLatexmkNoise file

    consumeLatexmkNoise root =
      PT.manyTill
        anyChar
        ( PT.try $
            PT.lookAhead $
              char '('
                >> string root
        )
        >> PT.getPosition
        >>= \pos ->
          return $
            MultiMessage
              [ AppMsg $ AppMessage "Rerun detected" pos DebugMsg,
                RerunDetected
              ]

    logPop :: PT.SourcePos -> String -> ParseMessage
    logPop pos file = AppMsg $ AppMessage ("Popped: " `append` pack file) pos TraceMsg

    popFile st =
      case stackPop (files_ st) of
        Just (files, file) ->
          PT.setState (st {files_ = files}) >> return file

pageEnd =
  do_parse
    >>= updateState
    >>= \pg ->
      PT.getPosition
        >>= \pos ->
          return $
            Just $
              AppMsg $ AppMessage (pack $ "Beginning page: " ++ show pg) pos TraceMsg
  where
    do_parse = char '[' >> PT.many1 digit <* char ']'
    updateState new_pg =
      PT.modifyState (\st -> st {curr_page_ = (read new_pg) + 1})
        >> return new_pg

genericMsg = doParse
  where
    doParse =
      oneOfStr ["Package", "Document", "LaTeX"]
        >> chChoice
        >> string "info: "
        >> return Nothing
    chChoice = PT.manyTill word actualSpace

providesMsg = doParse
  where
    doParse =
      oneOfStr ["Document Class", "File", "Package"]
        >> string ": "
        >> consumeLine
        >> ( PT.getPosition >>= \pos ->
               return $
                 Just $
                   AppMsg $ AppMessage "Found provides" pos TraceMsg
           )

generalNoise = doParse >> return Nothing
  where
    doParse =
      char '\\'
        >> word
        >> char '\\'
        >> PT.skipMany1 lower
        >> PT.skipMany1 digit
        >> newline

{-
Printed when a runaway argument is detected, provides some tokens as context
and then calls print_err.
Format:
Runaway ...\n<list of tokens>\n! <Error message>

-}
runawayArgument =
  string "Runaway "
    >> consumeLine
    >> string "{"
    <> PT.many letter
      >>= \before ->
        PT.manyTill consumeLine (PT.try $ PT.lookAhead $ string "! ")
          >> Msg
            <$> reportWithStackTrace
              "Runaway argument"
              Nothing
              ErrMsg
              ( Just $
                  ErrorContext
                    []
                    (Just $ ErrorLocation (pack before) "")
              )

{-
Missing file for \include is not reported as an error (unlike with \input{}).
-}
missingInclude =
  string "No file "
    >> PT.manyTill anyChar (PT.try $ string ".tex")
    <* char '.'
    <* PT.endOfLine
    >>= \missing_fp ->
      Msg
        <$> reportMsg
          ("Missing include file: " ++ missing_fp ++ ".tex")
          Nothing
          WarnMsg

splitTupleList :: [(a, b)] -> ([a], [b])
splitTupleList = foldr doSep ([], [])
  where
    doSep (ta, tb) (la, ba) = (ta : la, tb : ba)

actualSpace :: Parser Char
actualSpace = char ' '
