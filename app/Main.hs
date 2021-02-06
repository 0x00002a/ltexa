module Main where

import qualified ArgParse as A
import Data.Text ( unpack)
import qualified Data.Text as T
import qualified IO as I
import qualified Parse as P
import PrettyPrint (prettyPrintAll)
import System.IO (stdout)
import qualified Types as TP

main :: IO ()
main = A.parseArgs >>= handleArgs

handleArgs (A.StandardTLA args) = parseInput >>= displayResults
  where
    parseInput = handleInput <$> (input >>= doParse)
      where
        input = I.readAll $ A.infile_ args
        handleInput contents
          | (T.null . T.strip) contents = Right [] -- Empty input
          | otherwise = P.parse contents
        doParse txt =
          if A.do_passthrough_ args
            then putStrLn (unpack txt) >> return txt
            else return txt
    displayResults (Left err) = putStr $ unpack err
    displayResults (Right messages) = prettyPrintAll (filterMsgs usedMessages) stdout (A.print_mode_ args)
      where
        usedMessages = case A.max_reruns_ args of
          Nothing -> messages 
          Just max -> filterMessages 0 max messages []

        filterMessages _ _ [] done = done
        filterMessages i max (m : ms) done
          | i == max =
            if null done 
              then []
              else tail (init done)
          | m == TP.RerunDetected = filterMessages (i + 1) max ms (done ++ [m])
          | otherwise = filterMessages i max ms (done ++ [m])

    filterMsgs = filter (\msg -> P.getMsgType msg >= A.log_level_ args)
handleArgs A.VersionTLA = I.printVersion
