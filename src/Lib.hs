{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Data.Text (pack)
import qualified Parse as P

someFunc :: IO ()
someFunc =
  pack <$> readFile "./in.txt" >>= P.parse

--  P.parse "Underfull \\hbox (badness 2818) in paragraph at lines 230--231"

--P.parse "qwjelqwlke\nwekwqjelqjwelkqjlke\nwqkejlwqkej\n! Error!\n l.23\n"
