module Types where

import Data.ByteString (ByteString)

data MessageType = ErrMsg | WarnMsg | InfoMsg | DebugMsg | TraceMsg deriving (Eq)

instance Ord MessageType where
  (<=) ErrMsg _ = False
  (<=) WarnMsg ErrMsg = True
  (<=) WarnMsg _ = False
  (<=) InfoMsg msg = msg < WarnMsg
  (<=) DebugMsg msg = msg < InfoMsg
  (<=) TraceMsg _ = True

data InFileType = StdinFT | PathST FilePath

type StreamT = ByteString
