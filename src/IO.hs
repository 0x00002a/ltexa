{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module IO where

import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Debug.Trace (trace)
import PrettyPrint (PrettyPrintable (..), rstrip, surround)
import qualified PrettyPrint as PP
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
    pVer = putStr VERSION_ltexa
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
              (-3)
              ( C.text (unpack after)
                  C.<$> C.magenta (C.text "~~^~~")
              )
        )
        <$$> C.text "column:"
        <+> C.int calcCol
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
      printBody "" = C.empty
      printBody txt =
        C.text $ unpack $ flatten txt
      printProviders = C.blue $ case providers of
        Nothing -> C.empty
        Just pvs ->
          foldl1
            (<>)
            ( map (surround (C.char '[') (C.char ']') . C.text . unpack) $
                filter (not . T.null) $ map T.strip pvs
            )
            <> C.text ": "

      path = case fp of
        Just p -> C.bold $ formatPath p <> C.colon
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

formatPath :: String -> C.Doc
formatPath txt = C.text $ stripPrefix txt
  where
    stripPrefix ('.' : '/' : rest) = rest
    stripPrefix str = str

maybeFormat :: PrettyPrintable a => Maybe a -> C.Doc
maybeFormat Nothing = C.empty
maybeFormat (Just p) = formatDoc p

flatten = T.replace "\n" " "

removeProviders :: [Text] -> Text -> Text
removeProviders pvs txt = foldl doReplace txt $ filter (not . T.null) $ map T.strip pvs
  where
    doReplace repl_str prov_name = T.replace (wrapProvider prov_name) T.empty repl_str
    wrapProvider prov = "(" `T.append` prov `T.append` ")"
