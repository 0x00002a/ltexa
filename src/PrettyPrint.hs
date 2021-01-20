{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint where

import Data.Text (Text, append)
import qualified Data.Text as T
import System.IO (Handle, hIsTerminalDevice, hPutStr)
import qualified Text.PrettyPrint.ANSI.Leijen as C
import qualified Types as TP

class PrettyPrintable a where
  formatDoc :: a -> C.Doc

  prettyPrintAll :: [a] -> Handle -> TP.ColourMode -> IO ()
  prettyPrintAll ps handle mode = docs mode >>= C.hPutDoc handle
    where
      rs = C.vsep $ map formatDoc ps
      docs TP.NoneCM = return $ C.plain rs
      docs TP.ForcedCM = return rs
      docs TP.AutoCM = pickColourMode handle >>= docs

rstrip :: Text -> Text
rstrip txt = doFold splitLines
  where
    doFold = foldr (\txt curr -> curr `append` " " `append` txt) ""
    splitLines = filter nEmpty $ T.lines txt
    nEmpty "" = False
    nEmpty _ = True

stripMultilined :: Text -> Text
stripMultilined txt = T.unwords $ map T.strip (T.lines txt)

surround :: C.Doc -> C.Doc -> C.Doc -> C.Doc
surround lhs rhs d = lhs <> d <> rhs

isatty :: Handle -> IO Bool
isatty = hIsTerminalDevice

pickColourMode :: Handle -> IO TP.ColourMode
pickColourMode hdl =
  isatty hdl >>= \is_term ->
    return $
      if is_term
        then TP.ForcedCM
        else TP.NoneCM
