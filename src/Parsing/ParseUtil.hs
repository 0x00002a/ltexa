{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Parsing.ParseUtil where
import qualified Text.Megaparsec as PT
import Parsing.PState
import Text.Megaparsec.Char
import Data.Text (Text, append, find, isInfixOf, pack)
import Data.Maybe (fromMaybe)
import Types
import Text.Megaparsec ((<|>), try)

foldMany :: Monad f => (a -> f (Maybe a)) -> a -> f a
foldMany func s = func s >>= maybeNext
    where
        maybeNext Nothing = return s
        maybeNext (Just st) = foldMany func st

word :: Parser String
word = PT.some letterChar

transformErr :: Either String a -> Parser a
transformErr (Left err) = fail err
transformErr (Right v) = return v

anyExceptNl :: Parser Char
anyExceptNl = PT.satisfy pred
  where
    pred '\n' = False
    pred _ = True


actualSpace :: Parser Char
actualSpace = char ' '

anyChar :: (PT.MonadParsec e s m, PT.Token s ~ Char) => m Char
anyChar = PT.anySingle


text :: Text -> Parser Text
text = PT.chunk


optionally :: (a -> Parser (Maybe a)) -> (a -> Parser a) -> a -> Parser a
optionally opt after s = (fromMaybe s <$> opt s) >>= after

optionally' :: (a -> Parser a) -> (a -> Parser a) -> a -> Parser a
optionally' opt = optionally (PT.optional . PT.try . opt)

(#>) :: Monad f => f a -> (b -> f b) -> (b -> f b)
x #> f = \v -> x >> f v

lines :: Parser [Text]
lines = PT.many "\n"

splitTupleList :: [(a, b)] -> ([a], [b])
splitTupleList = foldr doSep ([], [])
  where
    doSep (ta, tb) (la, ba) = (ta : la, tb : ba)

consumeLine :: Parser Text
consumeLine = pack <$> PT.manyTill anyChar eol

consumeLine_ :: Parser ()
consumeLine_ = () <$ PT.skipManyTill anyChar eol


(><>) :: (Monad f, Semigroup a) => f a -> f a -> f a
l ><> r = l >>= \x -> (x <>) <$> r


-- |
--    Eats the input up until the first file start.
--    Prevents weirdness from trying to parse the introduction
--    which varies between implementations
upToFirstFile :: Parser ParseMessage
upToFirstFile =
  PT.skipManyTill consumeLine_ fStart
    >> makeMsg <$> PT.getSourcePos
  where
    makeMsg pos =
      AppMsg $
        AppMessage "Consumed introduction" pos DebugMsg
    fStart =
      PT.try $
        PT.lookAhead $
          char '('
            >> PT.skipManyTill PT.anySingle ((PT.try newline) <|> char ')')

oneOfStr xs = PT.choice $ string `map` xs

manyTillLH p sep =
  PT.manyTill p (PT.lookAhead sep)
    <> sep



