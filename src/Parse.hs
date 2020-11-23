{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.Functor.Identity
import Data.Maybe (catMaybes)
import Data.Stack
import Data.Text (Text, append, find, isInfixOf, pack, unpack)
import Debug.Trace
import qualified PrettyPrint as PP
import Text.Parsec ((<|>))
import qualified Text.Parsec as PT
import Text.Parsec.Char
import Types

parse :: StreamT -> Either Text [ParseMessage]
parse txt = handleResult $ PT.runParser parseLtexOutput freshState "src" txt
  where
    handleResult parser = case parser of
      Left err -> Left $ pack $ show err
      Right xs -> Right $ catMaybes xs

class TypedMessage a where
  getMsgType :: a -> MessageType

instance TypedMessage AppMessage where
  getMsgType = app_msg_type_

instance TypedMessage ParseMessageData where
  getMsgType = msg_type_

instance TypedMessage ParseMessage where
  getMsgType (AppMsg msg) = getMsgType msg
  getMsgType (Msg msg) = getMsgType msg

data PState = PState
  { curr_page_ :: Integer,
    files_ :: Stack FilePath
  }

reportMsg name body num tp = build <$> PT.getState
  where
    build st =
      ParseMessageData
        (pack name)
        (pack body)
        (read <$> num)
        tp
        (curr_page_ st)
        ((stackPeek . files_) st)

freshState = PState {curr_page_ = 0, files_ = stackNew}

parseLtexOutput = PT.manyTill ltexParsers PT.eof

ltexParsers = PT.choice (base_parsers ++ [consumeNoise])
  where
    base_parsers = PT.try `map` expr_parsers
    expr_parsers =
      [ Just <$> error_msg,
        Just <$> badBox,
        Just <$> latex_warning,
        generalNoise,
        pg_end,
        genericMsg,
        providesMsg,
        fileStart,
        fileEnd
      ]

just_or :: a -> Maybe a -> a
just_or _ (Just val) = val
just_or val Nothing = val

consumeNoise = anyChar >> return Nothing

consumeLine = PT.manyTill anyChar newline

wrappedLine = loopOnLine ""
  where
    loopOnLine txt =
      PT.manyTill anyChar (PT.try $ PT.lookAhead newline)
        >>= \chars ->
          PT.getPosition
            >>= \pos ->
              newline
                >> if PT.sourceColumn pos >= 80
                  then loopOnLine $ txt ++ chars
                  else return $ txt ++ chars

--overWrapLine = ((79 <=) . PT.sourceColumn) <$> PT.getPosition

word = PT.many1 letter <* (PT.try $ PT.notFollowedBy letter)

oneOfStr xs = PT.choice $ string `map` xs

error_msg =
  char '!' *> space *> consumeLine
    >>= \name ->
      PT.manyTill anyChar (PT.try (string "l."))
        >>= \body ->
          PT.manyTill digit newline
            >>= \line ->
              Msg <$> reportMsg name body (Just line) ErrMsg

badBox = do_match >>= process
  where
    do_match = oneOfStr box_choices <> string " " <> box_match <> string " " <> string "("
    box_choices = ["Overfull", "Underfull", "Loose", "Tight"]
    box_match = string "\\" <> ((: []) <$> PT.oneOf "hv") <> string "box"
    process curr_msg =
      PT.choice
        [ found_message_desc,
          detected_message
        ]
        >>= \(line, msg) ->
          ( if "hbox" `isInfixOf` (pack (msg ++ curr_msg))
              then filterOffendingTxt
              else return ()
          )
            >>= (\_ -> Msg <$> reportMsg "" (curr_msg ++ msg) line WarnMsg)

    filterOffendingTxt =
      newline
        >> wrappedLine
        >> return ()
    found_message_desc =
      PT.manyTill
        anyChar
        ( PT.try
            ( string " in "
                >> oneOfStr ["paragraph", "alignment"]
                >> string " at lines "
            )
        )
        >>= \msg ->
          PT.manyTill digit (string "--")
            >>= \m1 ->
              PT.manyTill digit (PT.try $ PT.lookAhead $ PT.notFollowedBy digit)
                >>= \m2 -> return (Just (min m1 m2), msg)
    detected_message =
      PT.manyTill anyChar (PT.try $ string " while \\output is active")
        >>= \msg -> return $ (Nothing, msg)

latex_warning =
  string "LaTeX Warning: " >> PT.manyTill anyChar (PT.try udef)
    >>= \body ->
      PT.manyTill digit (PT.notFollowedBy digit)
        >>= \line -> Msg <$> reportMsg "" body (mEmpty line) WarnMsg
  where
    udef =
      PT.try (string "on input line ")
        <|> (string "\n" >> string "\n")
    mEmpty "" = Nothing
    mEmpty txt = Just txt

fileStart = doParse >>= updateState
  where
    doParse = PT.optional newline >> parseFName -- Either newline (name \n rest ) OR (name)
    parseFName =
      char '('
        >> PT.manyTill matched (PT.try $ PT.choice [newline, space, PT.lookAhead (char ')')])
        >>= \fname -> case fname of
          "" -> PT.unexpected "Err"
          val -> return val
    matched = satisfy notIgnored
    ignoredSet = "(){} \n"
    notIgnored str = case find (str ==) ignoredSet of
      Just _ -> False
      Nothing -> True

    updateState fname =
      PT.getPosition
        >>= \pos ->
          PT.modifyState (\st -> st {files_ = stackPush (files_ st) fname})
            >> (return $ Just . AppMsg $ AppMessage ("Pushed: " `append` (pack fname)) pos TraceMsg)

fileEnd = doParse >> updateState
  where
    doParse =
      char ')'
    updateState =
      PT.getPosition >>= \pos ->
        PT.getState
          >>= \st ->
            Just . AppMsg
              <$> if stackIsEmpty (files_ st)
                then return $ errReport pos
                else logPop pos <$> popFile st
    errReport :: PT.SourcePos -> AppMessage
    errReport pos = AppMessage "Extra ) in log" pos WarnMsg

    logPop :: PT.SourcePos -> String -> AppMessage
    logPop pos file = AppMessage ("Popped: " `append` (pack file)) pos TraceMsg

    popFile st =
      case stackPop (files_ st) of
        Just (files, file) ->
          PT.setState (st {files_ = files}) >> return file

pg_end = do_parse >>= updateState >> return Nothing
  where
    do_parse = char '[' >> PT.many1 digit <* (PT.notFollowedBy digit)
    updateState new_pg = PT.modifyState (\st -> st {curr_page_ = (read new_pg) + 1})

genericMsg = doParse
  where
    doParse =
      oneOfStr ["Package", "Document", "LaTeX"]
        >> PT.many1 chChoice <* (PT.try (PT.notFollowedBy chChoice))
        >> string "info: "
        >> return Nothing
    chChoice = PT.choice [word, string " "]

providesMsg = doParse
  where
    doParse =
      oneOfStr ["Document Class", "File", "Package"]
        >> string ": "
        >> return Nothing

generalNoise = doParse >> return Nothing
  where
    doParse =
      char '\\'
        >> word
        >> char '\\'
        >> PT.skipMany1 lower <* (PT.notFollowedBy lower)
        >> PT.skipMany1 digit <* PT.notFollowedBy digit
        >> newline
