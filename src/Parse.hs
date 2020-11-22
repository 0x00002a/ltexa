{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse (parse) where

import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import qualified PrettyPrint as PP
import Text.Parsec ((<|>))
import qualified Text.Parsec as PT
import Text.Parsec.Char
import Text.Parsec.Text

data PState = PState
  { curr_col :: Integer,
    contents :: Text
  }

data ParseMessageData = ParseMessageData
  { title_ :: Text,
    body_ :: Text,
    line_num_ :: Maybe Integer,
    msg_type_ :: MessageType
  }
  deriving (Show)

instance PP.PrettyPrintable ParseMessage where
  pretty_print (Msg (ParseMessageData title body line tp)) =
    putStr prefix
      >> printLine
      >> (putStr $ unpack body)
    where
      prefix = case tp of
        ErrMsg -> "Error"
        WarnMsg -> "Warning"
      printLine = case line of
        Just ln -> putStr $ " (" ++ show ln ++ "): "
        Nothing -> putStr ": "

data MessageType = ErrMsg | WarnMsg deriving (Show)

data ParseMessage = Msg ParseMessageData | Noise deriving (Show)

parse :: Text -> IO ()
parse txt = printParsed $ PT.parse parseLtexOutput "src" txt
  where
    printParsed parser = case parser of
      Left err -> print err
      Right xs -> mapM_ PP.pretty_println $ catMaybes xs

parseLtexOutput = PT.manyTill ltexParsers PT.eof

ltexParsers = PT.choice (base_parsers ++ [consumeNoise])
  where
    base_parsers = map (PT.try . (Just <$>)) expr_parsers
    expr_parsers =
      [ error_msg,
        bad_box
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
              return $
                Msg $ ParseMessageData (pack name) (pack body) (Just (read line)) ErrMsg

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
        >>= \(line, msg) -> return $ Msg $ ParseMessageData "" (pack msg) (read <$> line) WarnMsg
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

-- (return $ Msg $ ParseMessageData "Test" "Testing" 2 WarnMsg)

parse_token :: PState -> Text -> PState
parse_token state@(PState {curr_col = 0}) tkn = case tkn of -- Newline match
  "! " -> err_msg
  _ -> state
  where
    err_msg = state
