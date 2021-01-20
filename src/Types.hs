module Types where

import Data.Text (Text)
import Text.Parsec (SourcePos)

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
    line_num_ :: Maybe Integer,
    msg_type_ :: MessageType,
    reported_page_ :: Integer,
    reported_file_ :: Maybe FilePath,
    err_ctx_ :: Maybe ErrorContext,
    providers_ :: Maybe [Text]
  }
  deriving (Show)

data ErrorContext = ErrorContext
  { stack_trace_ :: [Text],
    err_at_ :: Maybe ErrorLocation
  }
  deriving (Show)

data ErrorLocation = ErrorLocation
  { err_before_ :: Text,
    err_after_ :: Text
  }
  deriving (Show)

data AppMessage = AppMessage
  { what_ :: Text,
    pos_ :: SourcePos,
    app_msg_type_ :: MessageType
  }
  deriving (Show)

data ParseMessage
  = Msg ParseMessageData
  | AppMsg AppMessage
  | RerunDetected
  deriving (Show)

instance Show MessageType where
  show tp = case tp of
    ErrMsg -> "error"
    WarnMsg -> "warning"
    InfoMsg -> "info"
    DebugMsg -> "debug"
    TraceMsg -> "trace"
