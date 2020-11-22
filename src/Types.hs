module Types where

data MessageType = ErrMsg | WarnMsg | InfoMsg | DebugMsg | TraceMsg deriving (Ord, Eq)
