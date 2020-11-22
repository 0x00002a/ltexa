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
    reported_page_ :: Integer
  }
  deriving (Show)

instance PP.PrettyPrintable ParseMessage where
  pretty_print (Msg (ParseMessageData title body line tp page)) =
    putStr prefix
      >> printLine
      >> printPage
      >> (putStr $ unpack body)
    where
      prefix = case tp of
        ErrMsg -> "Error"
        WarnMsg -> "Warning"
      printLine = case line of
        Just ln -> putStr $ " (" ++ show ln ++ "): "
        Nothing -> putStr ": "
      printPage = putStr $ show page

data MessageType = ErrMsg | WarnMsg deriving (Show)

data ParseMessage = Msg ParseMessageData | Noise deriving (Show)

reportMsg name body num tp =
  ParseMessageData
    (pack name)
    (pack body)
    (read <$> num)
    tp
    <$> (curr_page_ <$> PT.getState)

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
        Just <$> latex_warning
      ]

just_or :: a -> Maybe a -> a
just_or _ (Just val) = val
just_or val Nothing = val

consumeNoise = anyChar >> return Nothing

consumeLine = PT.manyTill anyChar newline

oneOfStr xs = PT.choice $ string `map` xs

error_msg =
  char '!' *> space *> consumeLine
    >>= \name ->
      PT.manyTill anyChar (PT.try (string "l."))
        >>= \body ->
          PT.manyTill digit newline
            >>= \line ->
              Msg <$> reportMsg name body (Just line) ErrMsg

bad_box = do_match >> process
  where
    do_match = oneOfStr box_choices <* space <* box_match <* space <* char '('
    box_choices = ["Overfull", "Underfull", "Loose", "Tight"]
    box_match = char '\\' >> PT.oneOf "hv" >> string "box"
    process =
      PT.choice
        [ found_message_desc,
          detected_message
        ]
        >>= \(line, msg) -> Msg <$> reportMsg "" msg line WarnMsg
    found_message_desc =
      PT.manyTill
        anyChar
        ( PT.try
            ( string " in "
                >> oneOfStr ["paragraph", "alignment"]
                >> string " at lines "
            )
        )
        >> PT.manyTill digit (string "--")
        >>= \m1 ->
          PT.manyTill digit (PT.notFollowedBy digit)
            >>= \m2 -> return (Just (min m1 m2), "Placeholder")
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

pg_end = do_parse >>= updateState >> return Nothing
  where
    do_parse = char '[' >> PT.many1 digit <* (PT.notFollowedBy digit)
    updateState new_pg = PT.modifyState (\st -> st {curr_page_ = (read new_pg) + 1})
