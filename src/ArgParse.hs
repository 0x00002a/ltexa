module ArgParse
  ( parseArgs,
    TopLevelArgs (..),
    StandardArgs (..),
  )
where

import Data.Semigroup ((<>))
import Options.Applicative
import Types (InFileType (..)
    , MessageType (..), 
    OutFileType(..))

data TopLevelArgs = StandardTLA StandardArgs | VersionTLA

data StandardArgs = StandardArgs
  { log_level_ :: MessageType,
    infile_ :: InFileType,
    app_log_outfile_ :: OutFileType,
    latex_log_outfile :: OutFileType,
  }

parseArgs :: IO TopLevelArgs
parseArgs = execParser argsInfo

argsInfo =
  info (allArgs <**> helper) $
    progDesc "Parse and pretty print LaTeX output"

allArgs = mainArgs <|> versionOpt

versionOpt =
  pure VersionTLA
    <$> switch
      ( long "version"
          <> short 'V'
          <> help "Print version info"
      )

mainArgs =
  StandardTLA
    <$> ( StandardArgs
            <$> option
              (eitherReader parseLogLevel)
              ( long "log-level"
                  <> help "Set minimum log level"
                  <> value Types.InfoMsg
                  <> completer (listCompleter logLevels)
              )
            <*> argument
              (eitherReader parseInFile)
              ( value StdinFT
                  <> help "Log file to parse or (-) for stdin"
                  <> completer (bashCompleter "file")
                  <> metavar "INPUT"
              )
              <*> optional (
                  option (eitherReader parseOutFile) (
                    long "app-log-out"
                    <> help "Redirect app log output"
                      )
                  )
            <*> optional (
                option (eitherReader parseOutFile) (
                    long "latex-log-out"
                    <> help "Redirect latex log output"
                    )
        )
        )

parseInFile "-" = Right StdinFT
parseInFile path = Right $ PathST path


parseOutFile "-" = Right StdoutFT
parseOutFile path = Right $ PathSTO path



logLevels = ["debug", "info", "warn", "error", "trace"]

parseLogLevel "debug" = Right DebugMsg
parseLogLevel "info" = Right Types.InfoMsg
parseLogLevel "warn" = Right WarnMsg
parseLogLevel "error" = Right ErrMsg
parseLogLevel "trace" = Right TraceMsg
parseLogLevel _ = Left "Invalid option for log level"
