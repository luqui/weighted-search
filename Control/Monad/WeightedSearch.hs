-- | This is a nondeterminism monad which allows you to give computations
-- weights, such that the lowest-weight computations will be returned first.
-- This allows you to search infinite spaces productively, by guarding
-- recursive calls with weights.  Example:
--
-- > import qualified Control.Monad.WeightedSearch as W
-- > import Control.Applicative
-- > 
-- > -- All naturals, weighted by the size of the number
-- > naturals :: W.T Integer Integer
-- > naturals = go 0
-- >     where
-- >     go n = pure n <|> W.weight 1 (go $! n+1)
-- > 
-- > -- All finite lists, weighted by the length of the list
-- > finiteLists :: W.T Integer a -> W.T Integer a
-- > finiteLists = pure [] <|> W.weight 1 ((:) <$> w <*> finiteLists w)
-- > 
-- > -- A list of all finite lists of naturals
-- > finiteListsOfNaturals = W.toList (finiteLists naturals)
-- >    -- [ [], [0], [0,0], [1], [0,0,0], [0,1], [1,0], [2], [0,0,0,0], [0,0,1], ... ]
--
-- Weights must be strictly positive for this to be well-defined.

module Control.Monad.WeightedSearch 
    ( T, Weight(..), weight, toList )
where

import Control.Applicative
import Control.Monad (ap, MonadPlus(..))
import Control.Arrow (first)
import Data.Ratio (Ratio)
import Data.Foldable (Foldable, foldMap, toList)
import Data.Traversable (Traversable, sequenceA)
import Data.Monoid (Monoid(..))

-- | Weighted nondeterminstic computations over the weight @w@.  
data T w a
    = Fail
    | Yield a (T w a)
    | Weight w (T w a)

-- | The class of positive weights. We need to know how to subtract.  Weights
-- must be strictly positive.
class (Ord w) => Weight w where
    difference :: w -> w -> w

-- | Take a positive weight and weight a computation with it.
weight :: w -> T w a -> T w a
weight = Weight

instance Weight Int where difference = (-)
instance Weight Integer where difference = (-)
instance Weight Float where difference = (-)
instance Weight Double where difference = (-)
instance (Integral a) => Weight (Ratio a) where difference = (-)

instance Functor (T w) where
    fmap _ Fail = Fail
    fmap f (Yield x w) = Yield (f x) (fmap f w)
    fmap f (Weight a w) = Weight a (fmap f w)

instance (Weight w) => Monad (T w) where
    return x = Yield x Fail
    Fail >>= _ = Fail
    Yield x m >>= f = f x `mplus` (m >>= f)
    Weight w m >>= f = Weight w (m >>= f)

instance (Weight w) => MonadPlus (T w) where
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

instance (Weight w) => Applicative (T w) where
    pure = return
    (<*>) = ap

instance (Weight w) => Alternative (T w) where
    empty = mzero
    (<|>) = mplus

instance Foldable (T w) where
    foldMap _ Fail = mempty
    foldMap f (Yield a ms) = f a `mappend` foldMap f ms
    foldMap f (Weight _ w) = foldMap f w

instance Traversable (T w) where
    sequenceA Fail = pure Fail
    sequenceA (Yield x w) = Yield <$> x <*> sequenceA w
    sequenceA (Weight a w) = Weight a <$> sequenceA w
