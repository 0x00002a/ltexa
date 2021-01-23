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
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module IO where

import qualified Constants as CO
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Debug.Trace (trace)
import PrettyPrint (DocStyle, PrettyPrintable (..), rstrip, surround)
import qualified PrettyPrint as PP
import Prettyprinter
  ( (<+>),
  )
import qualified Prettyprinter as C
import qualified Prettyprinter.Render.Terminal as RT
import System.IO
  ( Handle (..),
    IOMode (..),
    hGetContents,
    stderr,
    stdin,
    stdout,
    withFile,
  )
import Text.Parsec
  ( SourcePos,
    sourceColumn,
    sourceLine,
  )
import Types

readAll :: InFileType -> IO Text
readAll StdinFT = pack <$> getContents
readAll (PathST fp) = pack <$> readFile fp

printVersion :: IO ()
printVersion = putStrLn CO.versionStr

instance PrettyPrintable ErrorContext where
  formatDoc (ErrorContext strace at) =
    maybeFormat at
      <> maybePrintBacktrace
    where
      maybePrintBacktrace =
        case strace of
          [] -> C.emptyDoc
          bt ->
            C.line'
              <> C.pretty "Backtrace:"
              <> C.hardline
              <> C.indent 4 (printBacktrace bt)
              <> C.line'
      printBacktrace bt = C.vcat $ map traceLine bt
      traceLine line_txt =
        C.pretty "-" <+> C.pretty (unpack line_txt)

instance PrettyPrintable ErrorLocation where
  formatDoc (ErrorLocation before after) =
    C.pretty "At:"
      <> C.hang
        4
        ( C.line'
            <> C.pretty before
            <> C.hang
              (-3)
              ( C.pretty after
                  <> C.line
                  <> C.annotate (RT.color RT.Magenta) "~~^~~"
              )
        )
      <> C.line'
      <> C.pretty ("column:" :: Text)
      <+> C.pretty calcCol
    where
      calcCol = T.length after + T.length before

instance PrettyPrintable ParseMessage where
  formatDoc (Msg (ParseMessageData body line tp page fp strace providers)) =
    printProviders
      <> path
      <> printLine
      <> formatDoc tp
      -- body may contain newlines
      <> printBody (PP.stripMultilined $ maybe body (`removeProviders` body) providers)
      <> printPage
      <> printStackTrace
    where
      printBody "" = C.emptyDoc
      printBody txt =
        C.pretty $ unpack $ flatten txt
      printProviders = C.annotate (RT.color RT.Blue) $ case providers of
        Nothing -> C.emptyDoc
        Just pvs ->
          foldl1
            (<>)
            ( map (surround (C.pretty '[') (C.pretty ']') . C.pretty . unpack) $
                filter (not . T.null) $ map T.strip pvs
            )
            <> C.pretty (": " :: Text)

      path = case fp of
        Just p -> C.annotate RT.bold $ formatPath p <> C.colon
        Nothing -> C.emptyDoc

      printLine = case line of
        Just ln ->
          C.annotate
            (RT.bold <> RT.color RT.Cyan)
            $ C.pretty ln
              <> C.pretty (": " :: Text)
        Nothing -> C.pretty $ pack " "
      printPage = C.pretty $ pack $ " (page " ++ show page ++ ")"
      printStackTrace = case strace of
        Nothing -> C.emptyDoc
        Just bt -> C.line' <> formatDoc bt
  formatDoc (AppMsg (AppMessage what pos tp)) =
    prefix
      <+> pType
      <+> msg
      <+> location
    where
      prefix =
        C.annotate
          (RT.bold <> RT.color RT.Magenta)
          $ C.pretty ("[LTeXa]:" :: Text)
      pType = formatDoc tp
      location = formatDoc pos
      msg = C.pretty what
  formatDoc (MultiMessage msgs) = C.vsep $ map formatDoc msgs
  formatDoc RerunDetected =
    C.hardline
      <> C.pretty " == NEW LOG == "
      <> C.hardline

instance PrettyPrintable SourcePos where
  formatDoc pos =
    C.pretty $
      "(line: " ++ show (sourceLine pos)
        ++ ", col: "
        ++ show (sourceColumn pos)
        ++ ")"

instance PrettyPrintable MessageType where
  formatDoc msg =
    C.annotate (RT.bold <> chooseColour) (C.pretty $ pack $ show msg ++ ": ")
    where
      chooseColour = case msg of
        ErrMsg -> RT.colorDull RT.Red
        WarnMsg -> RT.colorDull RT.Magenta
        InfoMsg -> RT.colorDull RT.Blue
        DebugMsg -> RT.color RT.Green
        TraceMsg -> RT.colorDull RT.White

formatPath :: String -> C.Doc DocStyle
formatPath txt = C.pretty $ stripPrefix txt
  where
    stripPrefix ('.' : '/' : rest) = rest
    stripPrefix str = str

maybeFormat :: PrettyPrintable a => Maybe a -> C.Doc DocStyle
maybeFormat Nothing = C.emptyDoc
maybeFormat (Just p) = formatDoc p

flatten = T.replace "\n" " "

removeProviders :: [Text] -> Text -> Text
removeProviders pvs txt = foldl doReplace txt $ filter (not . T.null) $ map T.strip pvs
  where
    doReplace repl_str prov_name = T.replace (wrapProvider prov_name) T.empty repl_str
    wrapProvider prov = "(" `T.append` prov `T.append` ")"
