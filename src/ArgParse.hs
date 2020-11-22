module ArgParse
  ( parseArgs,
    TopLevelArgs (..),
    LogLevel (..),
  )
where

import Data.Semigroup ((<>))
import Options.Applicative
import Types (MessageType (..))

data TopLevelArgs = TopLevelArgs
  { log_level_ :: MessageType
  }

parseArgs :: IO TopLevelArgs
parseArgs = execParser mainArgs

mainArgs =
  TopLevelArgs
    <$> option
      (eitherReader parseLogLevel)
      ( long "log-level" <> help "Set minimum log level" <> value LogInfo
      )

parseLogLevel "debug" = Just DebugMsg
parseLogLevel "info" = Just InfoMsg
parseLogLevel "warn" = Just WarnMsg
parseLogLevel "error" = Just ErrMsg
parseLogLevel "trace" = Just TraceMsg
parseLogLevel _ = Nothing
