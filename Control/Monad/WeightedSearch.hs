{-# LANGUAGE RankNTypes #-}

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
    ( Thread )
where

import Control.Applicative
import Control.Monad (ap, MonadPlus(..))
import Control.Arrow (first)
import Control.Monad.Trans.State
import Data.Ratio (Ratio)
import Data.Foldable (Foldable, fold, foldMap, toList)
import Data.Traversable (Traversable, sequenceA)
import Data.Monoid (Monoid(..))
import qualified Data.PQueue.Min as PQ



newtype Thread r w a = Thread { runThread ::
       r             -- fail
    -> (a -> r -> r) -- return
    -> (w -> r -> r) -- block
    -> (r -> r -> r) -- fork 
    -> r }

instance Functor (Thread r w) where
  fmap f m = Thread (\fail ret block fork -> runThread m fail (ret . f) block fork)

instance Monad (Thread r w) where
  return x = Thread (\_ ret _ _ -> ret x)
  m >>= f = Thread (\fail ret block fork ->
    runThread m fail (\x -> runThread (f x) fail ret block fork) block fork)

instance MonadPlus (Thread r w) where
  mzero = Thread (\fail _ _ _ -> fail)
  m `mplus` n = Thread (\fail ret block fork ->
    fork (runThread m fail ret block fork) (runThread n fail ret block fork))

instance Applicative (Thread r w) where
  pure = return
  (<*>) = ap

instance Alternative (Thread r w) where
  empty = mzero
  (<|>) = mplus



data Pri w a = Pri w a

instance (Eq w) => Eq (Pri w a) where
  Pri w _ == Pri w' _ = w == w'

instance (Ord w) => Ord (Pri w a) where
  compare (Pri w _) (Pri w' _) = compare w w'


type Queue r w = PQ.MinQueue (Pri w (Thread r w r))

sched :: (Ord w, Num w) => Queue r w -> [r]
sched q
  | Nothing <- qState = []
  | Just (Pri w thread, q') <- qState = 
    runThread thread (sched q')
                     (\x thread' -> 
                     (\w' thread' -> sched (PQ.insert (Pri (w+w') thread') q'))
                     (\t thread' -> sched (PQ.insert (Pri w thread')
                                           (PQ.insert (Pri w t) q')))
  where
  qState = PQ.minView q
