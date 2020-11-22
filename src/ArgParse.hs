module ArgParse
  ( parseArgs,
    TopLevelArgs (..),
  )
where

import Data.Semigroup ((<>))
import Options.Applicative
import Types (MessageType (..))

data TopLevelArgs = TopLevelArgs
  { log_level_ :: MessageType
  }

parseArgs :: IO TopLevelArgs
parseArgs = execParser mainArgsInfo

mainArgsInfo = info mainArgs (progDesc "Parse and pretty print LaTeX output")

mainArgs =
  TopLevelArgs
    <$> option
      (eitherReader parseLogLevel)
      ( long "log-level" <> help "Set minimum log level" <> value Types.InfoMsg
      )

parseLogLevel "debug" = Right DebugMsg
parseLogLevel "info" = Right Types.InfoMsg
parseLogLevel "warn" = Right WarnMsg
parseLogLevel "error" = Right ErrMsg
parseLogLevel "trace" = Right TraceMsg
parseLogLevel _ = Left "Invalid option for log level"
