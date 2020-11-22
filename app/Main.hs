module Main where

import qualified ArgParse as A
import Data.Text (Text, pack, unpack)
import qualified Parse as P
import PrettyPrint (pretty_println)

main :: IO ()
main = A.parseArgs >>= handleArgs

handleArgs args = parseInput >>= displayResults
  where
    parseInput = P.parse . pack <$> readFile "./in.txt"
    displayResults (Left err) = putStr $ unpack err
    displayResults (Right messages) = mapM_ pretty_println $ filterMsgs messages

    filterMsgs = filter (\msg -> (P.getMsgType msg) >= (A.log_level_ args))
