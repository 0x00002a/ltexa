module Util where


mapLeft :: (e -> b) -> Either e a -> Either b a
mapLeft f (Left v) = Left $ f v
mapLeft f (Right v) = Right v

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither v Nothing = Left v
maybeToEither _ (Just v) = Right v

showJust :: Show a => Maybe a -> String
showJust (Just v) = show v
showJust Nothing = undefined

