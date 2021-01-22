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
{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint where

import Data.Text (Text, append)
import qualified Data.Text as T
import qualified Prettyprinter as C
import System.IO (Handle, hIsTerminalDevice, hPutStr)
import qualified Types as TP
import qualified Prettyprinter.Render.Terminal as RT

type DocStyle = RT.AnsiStyle

class PrettyPrintable a where
  formatDoc :: a -> C.Doc DocStyle 

  prettyPrintAll :: [a] -> Handle -> TP.ColourMode -> IO ()
  prettyPrintAll ps handle mode = docs mode >>= RT.hPutDoc handle
    where
      rs = C.vsep $ map formatDoc ps
      docs TP.NoneCM = return $ C.unAnnotate rs
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

surround :: C.Doc DocStyle -> C.Doc DocStyle -> C.Doc DocStyle -> C.Doc DocStyle
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
