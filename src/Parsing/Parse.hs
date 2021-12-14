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
--{-# LANGUAGE TypeN }
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Parsing.Parse where

import Debug.Trace (trace)
import Control.Monad (void, when, (>=>))
import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as LNE
import Text.Megaparsec.Debug (dbg)
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Text (Text, append, find, isInfixOf, pack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Void
import qualified Debug.Trace
import Text.Megaparsec ((<|>), try)
import qualified Text.Megaparsec as PT
import Text.Megaparsec.Char
import Types

import Parsing.ParseUtil
import Parsing.Parsers
import Parsing.PState



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

parseLtexOutput :: PState -> Parser PState
parseLtexOutput st = (parseLtexSegment st) <* (PT.many "\n" >> PT.eof)
    where
        maybeParseInner :: PState -> Parser PState
        maybeParseInner = foldMany (PT.optional . try . ltexParsers)
        -- foldl (\xs x -> xs <> x) st
        parseLtexSegment = start >=> maybeParseInner >=> end
            where
                start st = fileStart st <* text "\n"
                end st = PT.many "\n" >> fileEnd st


ltexParsers :: PState -> Parser PState
ltexParsers st = PT.choice base_parsers
  where
    base_parsers = map (try . (\f -> f st)) expr_parsers
    expr_parsers =
      [ parseError,
        runawayArgument,
        badBox,
        latexWarning,
        missingInclude,
        pageEnd,
        genericMsg,
        providesMsg
        ]
        {- Just <$> ,
        Just <$> st,
        Just <$> st,
        generalNoise,
        st,
        st,
        st,
        (\_ -> Just st {at_eof_ = True}) <$> PT.eof
      ] -}

--consumeNoise = PT.anySingle


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


