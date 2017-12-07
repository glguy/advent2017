module Advent.Fix where

-- | Fixed-point of a type
newtype Fix f = Fix (f (Fix f))

-- | Generic fold
cata :: Functor t => (t a -> a) -> Fix t -> a
cata f (Fix x) = f (fmap (cata f) x)

-- | Generic monadic fold
cataM :: Monad m => Traversable t => (t a -> m a) -> Fix t -> m a
cataM f (Fix x) = f =<< traverse (cataM f) x

-- | Generic unfold
ana :: Functor f => (a -> f a) -> (a -> Fix f)
ana f = Fix . fmap (ana f) . f
