module Types where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Text.Parsec (SourcePos)

data MessageType = ErrMsg | WarnMsg | InfoMsg | DebugMsg | TraceMsg deriving (Eq)

instance Ord MessageType where
  (<=) ErrMsg _ = False
  (<=) WarnMsg ErrMsg = True
  (<=) WarnMsg _ = False
  (<=) InfoMsg msg = msg < WarnMsg
  (<=) DebugMsg msg = msg < InfoMsg
  (<=) TraceMsg _ = True

data InFileType = StdinFT | PathST FilePath

data OutFileType = StdoutFT | StdLogFT | PathSTO FilePath

type StreamT = Text

data ParseMessageData = ParseMessageData
  { title_ :: Text,
    body_ :: Text,
    line_num_ :: Maybe Integer,
    msg_type_ :: MessageType,
    reported_page_ :: Integer,
    reported_file_ :: Maybe FilePath
  }
  deriving (Show)

data AppMessage = AppMessage
  { what_ :: Text,
    pos_ :: SourcePos,
    app_msg_type_ :: MessageType
  }
  deriving (Show)

data ParseMessage = Msg ParseMessageData | AppMsg AppMessage deriving (Show)

instance Show MessageType where
  show tp = case tp of
    ErrMsg -> "error"
    WarnMsg -> "warning"
    InfoMsg -> "info"
    DebugMsg -> "debug"
    TraceMsg -> "trace"
