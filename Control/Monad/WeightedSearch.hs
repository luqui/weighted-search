module Control.Monad.WeightedSearch 
    ( M, Weight(..), weight )
where

import Control.Applicative
import Control.Monad (ap, MonadPlus(..))
import Control.Arrow (first)
import Data.Ratio (Ratio)
import Data.Foldable (Foldable, foldMap)
import Data.Traversable (Traversable, sequenceA)
import Data.Monoid (Monoid(..))

-- | Weighted nondeterminstic computations over the weight @w@.  
data M w a
    = Fail
    | Yield a (M w a)
    | Weight w (M w a)

-- | The class of positive weights. We need to know how to subtract.
class (Ord w) => Weight w where
    difference :: w -> w -> w

-- | Take a *positive* weight and weight a computation with it.
weight :: w -> M w a -> M w a
weight = Weight

instance Weight Int where difference = (-)
instance Weight Integer where difference = (-)
instance Weight Float where difference = (-)
instance Weight Double where difference = (-)
instance (Integral a) => Weight (Ratio a) where difference = (-)

instance Functor (M w) where
    fmap _ Fail = Fail
    fmap f (Yield x w) = Yield (f x) (fmap f w)
    fmap f (Weight a w) = Weight a (fmap f w)

instance (Weight w) => Monad (M w) where
    return x = Yield x Fail
    Fail >>= _ = Fail
    Yield x m >>= f = f x `mplus` (m >>= f)
    Weight w m >>= f = Weight w (m >>= f)

instance (Weight w) => MonadPlus (M w) where
    mzero = Fail
    Fail `mplus` m = m
    Yield x m `mplus` n = Yield x (m `mplus` n)
    Weight w m `mplus` Fail = Weight w m
    Weight w m `mplus` Yield x n = Yield x (Weight w m `mplus` n)
    Weight w m `mplus` Weight w' n
        = case compare w w' of
            LT -> Weight w (m `mplus` Weight (difference w' w) n)
            EQ -> Weight w (m `mplus` n)
            GT -> Weight w' (Weight (difference w w') m `mplus` n)

instance (Weight w) => Applicative (M w) where
    pure = return
    (<*>) = ap

instance Foldable (M w) where
    foldMap _ Fail = mempty
    foldMap f (Yield a ms) = f a `mappend` foldMap f ms
    foldMap f (Weight _ w) = foldMap f w

instance Traversable (M w) where
    sequenceA Fail = pure Fail
    sequenceA (Yield x w) = Yield <$> x <*> sequenceA w
    sequenceA (Weight a w) = Weight a <$> sequenceA w
