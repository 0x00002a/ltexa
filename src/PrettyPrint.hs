{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint where

import Data.Text (Text, append)
import qualified Data.Text as T
import System.IO (Handle, hPutStr)
import qualified Text.PrettyPrint.ANSI.Leijen as C

class PrettyPrintable a where
  formatDoc :: a -> C.Doc
  prettyPrint :: a -> Handle -> IO ()
  prettyPrint p hd = C.hPutDoc hd $ formatDoc p
  prettyPrintLn :: a -> Handle -> IO ()
  prettyPrintLn p hd = prettyPrint p hd >> hPutStr hd "\n"
  prettyPrintAll :: [a] -> Handle -> IO ()
  prettyPrintAll ps handle = C.hPutDoc handle $ C.vsep $ map formatDoc ps

rstrip :: Text -> Text
rstrip txt = doFold splitLines
  where
    doFold = foldr (\txt curr -> curr `append` " " `append` txt) ""
    splitLines = filter nEmpty $ T.lines txt
    nEmpty "" = False
    nEmpty _ = True

stripMultilined :: Text -> Text
stripMultilined txt = T.unwords $ map T.strip (T.lines txt)

surround :: C.Doc -> C.Doc -> C.Doc
surround d s = s <> d <> s
