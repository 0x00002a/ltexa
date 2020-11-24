{-# LANGUAGE CPP #-}

module IO where

import qualified Data.ByteString as B
import Data.Text (Text, pack, unpack)
import PrettyPrint (PrettyPrintable (..), rstrip)
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
import Text.PrettyPrint.ANSI.Leijen
  ( (<+>),
    (<>),
  )
import qualified Text.PrettyPrint.ANSI.Leijen as C
import Types

readAll :: InFileType -> IO Text
readAll StdinFT = pack <$> hGetContents stdin
readAll (PathST fp) = pack <$> readFile fp

printVersion :: IO ()
printVersion =
  pInfo
    >> pVer
    >> putStrLn ""
  where
    pVer = putStr $ show VERSION_ltexa
    pInfo = putStr "LTeXa "

instance PrettyPrintable ParseMessage where
  formatDoc (Msg (ParseMessageData title body line tp page fp)) =
    path
      <> printLine
      <> formatDoc tp
      <> C.text (unpack $ rstrip body)
      <> printPage
    where
      path = case fp of
        Just p -> C.bold $ C.text $ p ++ ":"
        Nothing -> C.text ""

      printLine = case line of
        Just ln ->
          C.bold (C.cyan $ C.text $ show ln) <> C.text ": "
        Nothing -> C.text " "
      printPage = C.text $ " (page " ++ show page ++ ")"
  formatDoc (AppMsg (AppMessage what pos tp)) =
    prefix
      <+> pType
      <+> msg
      <+> location
    where
      prefix = C.bold $ C.magenta $ C.text "[LTeXa]:"
      pType = formatDoc tp
      location = formatDoc pos
      msg = C.text $ unpack what

instance PrettyPrintable SourcePos where
  formatDoc pos =
    C.text $
      "(line: " ++ (show (sourceLine pos))
        ++ ", col: "
        ++ (show (sourceColumn pos))
        ++ ")"

instance PrettyPrintable MessageType where
  formatDoc msg = C.bold $ chooseColour $ C.text $ show msg ++ ": "
    where
      chooseColour = case msg of
        ErrMsg -> C.red
        WarnMsg -> C.yellow
        InfoMsg -> C.blue
        DebugMsg -> C.green
        TraceMsg -> C.dullwhite
