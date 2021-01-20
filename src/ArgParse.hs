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
module ArgParse
  ( parseArgs,
    TopLevelArgs (..),
    StandardArgs (..),
  )
where

import qualified Constants as CO
import Data.Semigroup ((<>))
import Options.Applicative
import Types
  ( ColourMode (..),
    InFileType (..),
    MessageType (..),
    OutFileType (..),
  )

data TopLevelArgs = StandardTLA StandardArgs | VersionTLA

data StandardArgs = StandardArgs
  { log_level_ :: MessageType,
    infile_ :: InFileType,
    do_passthrough_ :: Bool,
    print_mode_ :: ColourMode
  }

parseArgs :: IO TopLevelArgs
parseArgs = customExecParser parserSettings argsInfo

parserSettings = prefs $ disambiguate <> showHelpOnEmpty

argsInfo =
  info (allArgs <**> helper) $
    progDesc "Parse and pretty print LaTeX output" <> header CO.versionStr

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
                  <> help "Set minimum log level (values: trace, debug, info, warn, error)"
                  <> value Types.InfoMsg
                  <> completeWith logLevels
                  <> showDefault
              )
              <*> argument
                (eitherReader parseInFile)
                ( value StdinFT
                    <> help "Log file to parse or (-) for stdin"
                    <> completer (bashCompleter "file")
                    <> metavar "INPUT"
                )
              <*> switch
                ( help "Print all input to output before parsed results"
                    <> long "pass-through"
                )
              <*> option
                auto
                ( help "Set colouring for output (values: none, forced, auto)"
                    <> long "colour"
                    <> value AutoCM
                    <> completeWith ["auto", "none", "forced"]
                    <> showDefault
                )
        )

parseInFile "-" = Right StdinFT
parseInFile path = Right $ PathST path

parseOutFile "-" = Right StdoutFT
parseOutFile path = Right $ PathSTO path

logLevels = ["debug", "info", "warn", "error", "trace"]

parseColourMode "auto" = AutoCM
parseColourMode "none" = NoneCM
parseColourMode "forced" = ForcedCM

parseLogLevel "debug" = Right DebugMsg
parseLogLevel "info" = Right Types.InfoMsg
parseLogLevel "warn" = Right WarnMsg
parseLogLevel "error" = Right ErrMsg
parseLogLevel "trace" = Right TraceMsg
parseLogLevel _ = Left "Invalid option for log level"
