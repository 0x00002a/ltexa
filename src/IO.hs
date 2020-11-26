{-# LANGUAGE CPP #-}

module IO where

import qualified Data.ByteString as B
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import PrettyPrint (PrettyPrintable (..), rstrip, surround)
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
  ( (<$$>),
    (<+>),
    (<//>),
    (</>),
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

instance PrettyPrintable ErrorContext where
  formatDoc (ErrorContext strace at) =
    maybeFormat at
      <> maybePrintBacktrace
    where
      maybePrintBacktrace =
        case strace of
          [] -> C.empty
          bt ->
            C.linebreak
              <> C.text "Backtrace:"
              <$$> C.indent 4 (printBacktrace bt)
              <> C.linebreak
      printBacktrace bt = C.vcat $ map traceLine bt
      traceLine line_txt =
        C.text "-" <+> C.text (unpack line_txt)

instance PrettyPrintable ErrorLocation where
  formatDoc (ErrorLocation before after) =
    C.text "At:"
      <> C.hang
        4
        ( C.linebreak
            <> C.text (unpack before)
            <> C.hang
              (-2)
              ( C.text (unpack after)
                  C.<$> C.magenta (C.text "~~^~~")
              )
        )
        <$$> C.text "column:"
        <+> C.int calcCol
    where
      calcCol = T.length after + T.length before

instance PrettyPrintable ParseMessage where
  formatDoc (Msg (ParseMessageData body line tp page fp strace)) =
    path
      <> printLine
      <> formatDoc tp
      <> C.text (unpack $ rstrip body)
      <> printPage
      <> printStackTrace
    where
      path = case fp of
        Just p -> C.bold $ C.text $ p ++ ":"
        Nothing -> C.text ""

      printLine = case line of
        Just ln ->
          C.bold (C.cyan $ C.text $ show ln) <> C.text ": "
        Nothing -> C.text " "
      printPage = C.text $ " (page " ++ show page ++ ")"
      printStackTrace = case strace of
        Nothing -> C.empty
        Just bt -> C.linebreak <> formatDoc bt
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
        ErrMsg -> C.dullred
        WarnMsg -> C.dullmagenta
        InfoMsg -> C.dullblue
        DebugMsg -> C.green
        TraceMsg -> C.dullwhite

maybeFormat :: PrettyPrintable a => Maybe a -> C.Doc
maybeFormat Nothing = C.empty
maybeFormat (Just p) = formatDoc p
