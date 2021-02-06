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
{-# LANGUAGE ViewPatterns #-}

module Types where

import Data.List (stripPrefix)
import Data.Text (Text)
import Text.Megaparsec (SourcePos)

data MessageType
  = ErrMsg
  | WarnMsg
  | InfoMsg
  | DebugMsg
  | TraceMsg
  deriving (Eq)

instance Ord MessageType where
  (<=) ErrMsg _ = False
  (<=) WarnMsg ErrMsg = True
  (<=) WarnMsg _ = False
  (<=) InfoMsg msg = msg >= WarnMsg
  (<=) DebugMsg msg = msg >= InfoMsg
  (<=) TraceMsg _ = True

data InFileType = StdinFT | PathST FilePath

data OutFileType = StdoutFT | StdLogFT | PathSTO FilePath

type StreamT = Text

data ParseMessageData = ParseMessageData
  { body_ :: Text,
    line_num_ :: Maybe Int,
    msg_type_ :: MessageType,
    reported_page_ :: Int,
    reported_file_ :: Maybe FilePath,
    err_ctx_ :: Maybe ErrorContext,
    providers_ :: Maybe [Text]
  }
  deriving (Show, Eq, Ord)

data ErrorContext = ErrorContext
  { stack_trace_ :: [Text],
    err_at_ :: Maybe ErrorLocation
  }
  deriving (Show, Eq, Ord)

data ErrorLocation = ErrorLocation
  { err_before_ :: Text,
    err_after_ :: Text
  }
  deriving (Show, Eq, Ord)

data AppMessage = AppMessage
  { what_ :: Text,
    pos_ :: SourcePos,
    app_msg_type_ :: MessageType
  }
  deriving (Show, Eq, Ord)

data ParseMessage
  = Msg ParseMessageData
  | AppMsg AppMessage
  | RerunDetected
  | MultiMessage [ParseMessage]
  deriving (Show, Ord, Eq)

instance Show MessageType where
  show tp = case tp of
    ErrMsg -> "error"
    WarnMsg -> "warning"
    InfoMsg -> "info"
    DebugMsg -> "debug"
    TraceMsg -> "trace"

data ColourMode = ForcedCM | AutoCM | NoneCM

instance Read ColourMode where
  readsPrec _ (stripPrefix "auto" -> Just rs) = [(AutoCM, rs)]
  readsPrec _ (stripPrefix "none" -> Just rs) = [(NoneCM, rs)]
  readsPrec _ (stripPrefix "forced" -> Just rs) = [(ForcedCM, rs)]

instance Show ColourMode where
  show AutoCM = "auto"
  show ForcedCM = "forced"
  show NoneCM = "none"
