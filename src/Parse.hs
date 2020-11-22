{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse (parse) where

import Data.Maybe (catMaybes)
import Data.Stack
import Data.Text (Text, pack, unpack)
import qualified PrettyPrint as PP
import Text.Parsec ((<|>))
import qualified Text.Parsec as PT
import Text.Parsec.Char
import Text.Parsec.Text

data PState = PState
  { curr_page_ :: Integer,
    files_ :: Stack FilePath
  }

data ParseMessageData = ParseMessageData
  { title_ :: Text,
    body_ :: Text,
    line_num_ :: Maybe Integer,
    msg_type_ :: MessageType,
    reported_page_ :: Integer,
    reported_file_ :: Maybe FilePath
  }
  deriving (Show)

instance PP.PrettyPrintable ParseMessage where
  pretty_print (Msg (ParseMessageData title body line tp page fp)) =
    putStr path
      >> printLine
      >> putStr prefix
      >> (putStr $ unpack body)
      >> printPage
    where
      path = case fp of
        Just p -> p ++ ":"
        Nothing -> ""
      prefix = case tp of
        ErrMsg -> "Error: "
        WarnMsg -> "Warning: "
        InfoMsg -> "Info: "
      printLine = case line of
        Just ln -> putStr $ show ln ++ ": "
        Nothing -> return ()
      printPage = putStr $ " (on page: " ++ show page ++ ")"

data MessageType = ErrMsg | WarnMsg | InfoMsg deriving (Show)

data ParseMessage = Msg ParseMessageData | Noise deriving (Show)

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

parse :: Text -> IO ()
parse txt = printParsed $ PT.runParser parseLtexOutput freshState "src" txt
  where
    printParsed parser = case parser of
      Left err -> print err
      Right xs -> mapM_ PP.pretty_println $ catMaybes xs

parseLtexOutput = PT.manyTill ltexParsers PT.eof

ltexParsers = PT.choice (base_parsers ++ [consumeNoise])
  where
    base_parsers = map (PT.try) expr_parsers
    expr_parsers =
      [ Just <$> error_msg,
        Just <$> bad_box,
        pg_end,
        Just <$> latex_warning,
        genericMsg,
        providesMsg,
        fileEnd
      ]

just_or :: a -> Maybe a -> a
just_or _ (Just val) = val
just_or val Nothing = val

consumeNoise = anyChar >> return Nothing

consumeLine = PT.manyTill anyChar newline

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

bad_box = do_match >>= process
  where
    do_match = oneOfStr box_choices <> string " " <> box_match <> string " " <> string "("
    box_choices = ["Overfull", "Underfull", "Loose", "Tight"]
    box_match = char '\\' >> PT.oneOf "hv" >> string "box"
    process curr_msg =
      PT.choice
        [ found_message_desc,
          detected_message
        ]
        >>= \(line, msg) -> Msg <$> reportMsg "" (curr_msg ++ msg) line WarnMsg
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
              PT.manyTill digit (PT.notFollowedBy digit)
                >>= \m2 -> return (Just (min m1 m2), msg)
    detected_message =
      PT.manyTill anyChar (string " while \\output is active")
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
        >> PT.manyTill anyChar (PT.try $ PT.choice [newline, space])
        >>= \fname -> case fname of
          "" -> PT.unexpected "Err"
          val -> return val

    updateState fname =
      PT.modifyState (\st -> st {files_ = stackPush (files_ st) fname})
        >> (Just . Msg <$> reportMsg "" ("Added: " ++ fname) Nothing InfoMsg)

fileEnd = doParse >> updateState
  where
    doParse =
      PT.try
        ( PT.lookAhead fileStart
            >> PT.lookAhead (PT.many anyChar)
        )
        >> char ')'
    updateState =
      PT.getState >>= \st ->
        if (stackIsEmpty (files_ st))
          then errReport
          else popFile st >> Just <$> Msg <$> reportMsg "" "" Nothing WarnMsg
    errReport =
      Just
        <$> Msg
        <$> ( PT.sourceLine <$> PT.getPosition
                >>= \line -> reportMsg "" "Extra ) in log" ((Just . show) line) WarnMsg
            )

    popFile st =
      case stackPop (files_ st) of
        Just (files, _) -> PT.setState (st {files_ = files})

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
