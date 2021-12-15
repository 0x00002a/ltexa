{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Parsing.PState where
import qualified Text.Megaparsec as PT
import Data.Stack
import Data.Text (Text)
import qualified Data.Text as T
import Types
import qualified PrettyPrint as PP
import IO


type Parser = PT.Parsec [ParseMessage] Text
type ParseError = PT.ParseErrorBundle Text [ParseMessage]

data PState = PState
  { curr_page_ :: Int,
    files_ :: Stack FilePath,
    messages_ :: [ParseMessage],
    at_eof_ :: Bool
  } deriving(Show)

instance Monoid PState where
    mempty = freshState

instance Semigroup PState where
    (<>) (PState lp lf lms leof) (PState rp rf rms reof) =
        mempty {
            curr_page_ = max lp rp,
            files_ = foldl stackPush lf (popAll rf),
            messages_ = lms ++ rms,
            at_eof_ = leof || reof
            }

addMsg :: PState -> ParseMessage -> PState
addMsg st msg = addMsgs st [msg]

addMsgs :: PState -> [ParseMessage] -> PState
addMsgs st msg = st {messages_ = messages_ st ++ msg}



popAll :: Stack a -> [a]
popAll s = case stackPop s of
        Just (sp, v) -> v:popAll sp
        Nothing -> []

reportMsg st body num tp =
  reportWithStackTrace
    st
    body
    num
    tp
    Nothing

reportWithStackTrace st body num tp strace = buildMsg
  where
    buildMsg =
      ParseMessageData
        body
        num
        tp
        (curr_page_ st)
        ((stackPeek . files_) st)
        strace
        (Just ["LaTeX"])

freshState = PState {curr_page_ = 0, files_ = stackNew, messages_ = [], at_eof_ = False}


instance PT.ShowErrorComponent Text where
  showErrorComponent txt = T.unpack txt

instance PT.ShowErrorComponent [ParseMessage] where
  showErrorComponent = T.unpack . PP.prettyShowAll



