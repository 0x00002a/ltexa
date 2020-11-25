{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse where

import qualified Data.ByteString as B
import Data.Functor.Identity
import Data.Maybe (catMaybes)
import Data.Stack
import Data.Text (Text, append, find, isInfixOf, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Debug.Trace
import qualified PrettyPrint as PP
import Text.Parsec ((<|>))
import qualified Text.Parsec as PT
import Text.Parsec.Char
import Types

parse :: StreamT -> Either Text [ParseMessage]
parse txt =
  handleResult $
    PT.runParser parseLtexOutput freshState "src" $
      firstPass txt
  where
    handleResult parser = case parser of
      Left err -> Left $ pack $ show err
      Right xs -> Right $ catMaybes xs

firstPass :: Text -> Text
firstPass txt = case PT.parse doParse "" txt of
  Left err -> undefined
  Right res ->
    trace ("--First pass out--\n\n" ++ concat res ++ "\n\n-- END --") $
      pack $ concat res
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

data PState = PState
  { curr_page_ :: Integer,
    files_ :: Stack FilePath,
    root_file_ :: Maybe FilePath
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

freshState = PState {curr_page_ = 0, files_ = stackNew, root_file_ = Nothing}

parseLtexOutput = PT.manyTill ltexParsers PT.eof

ltexParsers = PT.choice (base_parsers ++ [consumeNoise])
  where
    base_parsers = PT.try `map` expr_parsers
    expr_parsers =
      [ Just <$> error_msg,
        Just <$> badBox,
        Just <$> latexWarning,
        generalNoise,
        pageEnd,
        genericMsg,
        providesMsg,
        fileEnd,
        fileStart
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
                  else return $ txt ++ chars ++ "\n"

--overWrapLine = ((79 <=) . PT.sourceColumn) <$> PT.getPosition

word = PT.many1 letter <* (PT.try $ PT.notFollowedBy letter)

--manywords = unwords <$> (PT.many letter `PT.sepEndBy` PT.many space)

oneOfStr xs = PT.choice $ string `map` xs

manyTillLH p sep =
  PT.manyTill p (PT.lookAhead sep)
    <> sep

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
        [ PT.try normalMsgDesc,
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
        >> consumeLine
        >> return ()
    normalMsgDesc =
      descChoices
        >>= \(line, msg_second) -> return (line, msg_second)
    descChoices =
      PT.choice
        [ PT.try paraChoice,
          PT.try detectedChoice
        ]
    paraChoice =
      string " in "
        <> oneOfStr ["paragraph", "alignment"]
        <> string " at lines "
        >>= \msg ->
          PT.manyTill digit (string "--")
            >>= \m1 ->
              PT.manyTill digit (PT.try $ PT.notFollowedBy digit)
                >>= \m2 -> return (Just (min m1 m2), msg)
    detectedChoice =
      consumeLine
        >>= \line ->
          if " detected at line " `isInfixOf` (pack line)
            then
              PT.manyTill digit (PT.try $ PT.notFollowedBy digit)
                >>= \m1 -> return (Just m1, line ++ m1)
            else PT.unexpected "Line does not contained detected message"

    detected_message =
      PT.manyTill anyChar (PT.try $ string " while \\output is active")
        >>= \msg -> return $ (Nothing, msg)

toByteString :: String -> B.ByteString
toByteString = encodeUtf8 . pack

--maybeWrapped :: String -> PT.Parsec t s String
maybeWrapped [] = undefined
maybeWrapped [x] = PT.parserBind (char x) (\ch -> return [ch])
maybeWrapped (x : xs) =
  PT.optional newline
    >> char x
    >>= \ch -> ([ch] ++) <$> maybeWrapped xs

latexWarning =
  chChoices
    >> ( PT.parserTraced "Rem:" $
           manyTillLH
             anyChar
             (string "Warning: ")
             >> ( PT.parserTraced "Rem2:" $
                    (PT.optionMaybe $ PT.try $ tryFindLine)
                      >>= \bd ->
                        PT.manyTill anyChar newline
                          >>= \body ->
                            PT.parserTraced "Rem3:" $ retrMsg body bd
                )
       )
  where
    upTo = (Just <$> nl) <|> PT.optionMaybe parseMsgLine
    parseMsgLine =
      udef
        >>= \body ->
          PT.manyTill digit (PT.notFollowedBy digit)
            >>= \line -> return (body, Just line)
    udef =
      string " on input line "
    nl = newline >> return ("", Nothing)
    tryFindLine =
      PT.manyTill anyChar udef
        >>= \body ->
          PT.manyTill digit (PT.notFollowedBy digit)
            >>= \line -> return (body, Just line)

    retrMsg existing (Just (body, line)) =
      Msg <$> reportMsg "" (existing ++ body) line WarnMsg
    retrMsg existing Nothing =
      Msg
        <$> reportMsg "" existing Nothing WarnMsg
    mEmpty "" = Nothing
    mEmpty txt = Just txt
    chChoices =
      oneOfStr ["LaTeX", "Package", "Class", "pdfTeX"]
        >> string " "

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
          PT.modifyState
            ( \st ->
                st
                  { files_ = stackPush (files_ st) fname,
                    root_file_ =
                      case root_file_ st of
                        Just f -> Just f
                        Nothing -> Just fname
                  }
            )
            >> return
              ( Just . AppMsg $
                  AppMessage ("Pushed: " `append` pack fname) pos TraceMsg
              )

fileEnd = doParse >> updateState
  where
    doParse =
      char ')'
    updateState =
      PT.getState
        >>= \st ->
          Just . AppMsg <$> generateMsg (files_ st) st
    generateMsg files st =
      PT.getPosition >>= \pos ->
        if stackIsEmpty files
          then return $ errReport pos --errReport pos
          else
            if stackSize files == 1
              then nextRun files <* popFile st
              else logPop pos <$> popFile st
    errReport :: PT.SourcePos -> AppMessage
    errReport pos = AppMessage "Extra ) in log" pos WarnMsg
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
        >>= \pos -> return $ AppMessage "Parsing rerun" pos DebugMsg

    logPop :: PT.SourcePos -> String -> AppMessage
    logPop pos file = AppMessage ("Popped: " `append` (pack file)) pos TraceMsg

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
    chChoice = PT.manyTill word space

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
