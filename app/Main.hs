module Main where

import qualified ArgParse as A
import Data.Text (Text, pack, unpack)
import qualified IO as I
import qualified Parse as P
import PrettyPrint (pretty_println)

main :: IO ()
main = A.parseArgs >>= handleArgs

handleArgs (A.StandardTLA args) = parseInput >>= displayResults
  where
    parseInput = P.parse <$> (I.readAll $ A.infile_ args)
    displayResults (Left err) = putStr $ unpack err
    displayResults (Right messages) = mapM_ pretty_println $ filterMsgs messages

    filterMsgs = filter (\msg -> (P.getMsgType msg) >= (A.log_level_ args))
handleArgs (A.VersionTLA) = I.printVersion
