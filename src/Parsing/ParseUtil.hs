{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Parsing.ParseUtil where
import qualified Text.Megaparsec as PT
import Parsing.PState
import Control.Monad (void)
import Text.Megaparsec.Char
import Data.Text (Text, append, find, isInfixOf, pack)
import Data.Maybe (fromMaybe)
import Types
import Text.Megaparsec ((<|>), try, (<?>))

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
optionally opt after s = opt s >>= after . fromMaybe s

optionally' :: (a -> Parser a) -> (a -> Parser a) -> a -> Parser a
optionally' opt = optionally (PT.optional . PT.try . opt)

(#>) :: Monad f => f a -> (b -> f c) -> (b -> f c)
x #> f = \v -> x >> f v

(<#) :: Applicative f => (b -> f c) -> f a -> (b -> f c)
f <# x = \v -> f v <* x

noise :: Parser ()
noise = PT.skipMany (try newline <|> char ' ')

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
        PT.lookAhead startOfFile


startOfFile :: Parser Text
startOfFile = (char '(' >> parseFName) <?> "new file"
    where
        pathStart = optionally' text (\s -> (s <>) <$> text "/") "."
        endOfName = try newline <|> try (char ' ') <|> PT.lookAhead (char ')')
        parseFName =
            pathStart ><> (pack <$> PT.manyTill anyChar endOfName)


oneOfStr xs = PT.choice $ string `map` xs

manyTillLH p sep =
  PT.manyTill p (PT.lookAhead sep)
    <> sep


-- |
-- Unwraps a wrapped line. Not perfect due since it may "unwrap" a line which
-- was never wrapped in the first place, but there is no way around that.
wrappedLine = loopOnLine ""
  where
    loopOnLine txt =
      PT.manyTill PT.anySingle (PT.choice [PT.eof, void $ PT.try $ PT.lookAhead newline])
        >>= \chars ->
          PT.getSourcePos
            >>= \pos ->
              PT.choice
                [ (\_ -> txt <> pack chars) <$> PT.eof,
                  (newline :: Parser Char)
                    >> if PT.unPos (PT.sourceColumn pos) >= 80
                      then loopOnLine $ txt `append` pack chars
                      else return $ txt `append` pack chars `append` "\n"
                ]



