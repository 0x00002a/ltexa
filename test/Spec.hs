{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import Data.Text (Text, unpack, pack, isInfixOf)
import Parse (parse, getMsgType)
import Types
import Debug.Trace (trace)

containsMsg :: [ParseMessage] -> Text -> Bool
containsMsg [] _ = False
containsMsg ((Msg (ParseMessageData txt _ _ _ _ _ _)):xs) s
    | s `isInfixOf` txt = True
    | otherwise = containsMsg xs s
containsMsg (_:xs) s = containsMsg xs s 

paraParseTxt :: IO Text
paraParseTxt = pack <$> readFile "test/ltexa_err.log"
paraParse = TestCase $ (parse <$> paraParseTxt) >>= (\rs -> case rs of 
                Left err -> assertFailure $ unpack err
                Right res -> assertBool "Output expected" $ containsMsg res "Paragraph ended before")
    where 
    --doFilter = filter (\msg -> getMsgType msg >= WarnMsg)
    expected = [
        Msg $ ParseMessageData "Illegal unit of measure (pt inserted)." Nothing ErrMsg 0 (Just "/usr/share/texmf-dist/tex/latex/standalone/standalone.cls") (Just $ ErrorContext ["<to be read again>"] Nothing) Nothing
        ]

tests = TestList [paraParse]

main = runTestTT tests


