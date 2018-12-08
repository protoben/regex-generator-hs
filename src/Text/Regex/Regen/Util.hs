module Text.Regex.Regen.Util where

import Control.Monad.Catch (Exception, MonadCatch, SomeException, try)
import Data.Either (either)

mwhen :: Monoid m => Bool -> m -> m
mwhen b m = if b then m else mempty

munless :: Monoid m => Bool -> m -> m
munless b m = if b then mempty else m

infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

infixr 3 &&&
(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p &&& q = \a -> p a && q a

infixr 2 |||
(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p ||| q = \a -> p a || q a

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

tryMaybe :: MonadCatch m => m a -> m (Maybe a)
tryMaybe m = hush <$> try' m
    where
    try' :: MonadCatch m => m a -> m (Either SomeException a)
    try' = try
