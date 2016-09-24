{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, TypeApplications, UndecidableInstances #-}

{-| Impartial games, calculating their nimbers, or Grundy numbers.

    See <https://en.wikipedia.org/wiki/impartial_game>.
-}

module Data.Game.Impartial.Impartial (Nimber, mex, Position(..), Normal(..), NormalPosition(..), HasNimber(..), Positional(..)) where

import Data.Set (fromList, member, Set)
import Data.Bits (xor)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Vector (Vector, (!), generate)
import Data.Semigroup (Max(..))
import Data.Coerce (coerce)
import Data.Game.Impartial.Injection (Injection(..))
import Data.Nimber (Nimber)
-- import qualified Data.MemoTrie as MemoTrie
-- import qualified Data.MemoCombinators as MemoCombinators

mex :: Set Nimber -> Nimber
mex s = fromJust (find (not . (`member` s)) [0..])

{-|
A positional must have the property that

> forall p,
> inject p >= 0 /\
> fold (: []) (inject p) = [p]
-}
class Injection (Base p) p => Positional p where
    type Base p
    {-|
    This is a catamorphism.
     -}
    cata :: (Base p -> Nimber) -> p -> Nimber
    {-|
    This is a sepcialization of Fold from the lens library
      -}
    fold :: Monoid m => (Base p -> m) -> p -> m

{-|
A position must have the property that

> forall p,
> max (map (getMax . fold (Max . inject)) (next (inject p))) < inject p
-}
class Positional p => Position p where
    type Next p
    next :: p -> [Next p]

{-|
  An impartial game under normal play convention has an nimber, according to Sprague-Grundy theorem.
-}
class HasNimber n where
    nimber :: n -> Nimber
{-|
  A wrapper for normal play convertion. This should used when the nimber of the current postion can be calculated without using the 'next' function
-}
newtype Normal p = Normal p

{-|
  A wrapper for normal play convertion. This should used when the nimber of the current postion is calculated using the 'next' function
-}
newtype NormalPosition p = NormalPosition p


instance (
      Injection (Base p) Int, Position (Base p),
      Positional p, Positional (Next (Base p)),
      Base p ~ Base (Next (Base p))) => HasNimber (NormalPosition p) where
    nimber (NormalPosition p) = nimsum p
        where nimsum :: p -> Nimber
              nimsum = cata (memo . inject)
              nimsumNext :: Next (Base p) -> Nimber
              nimsumNext = cata (memo . inject)
              nimber' :: Base p -> Nimber
              nimber' = mex . fromList . map nimsumNext . next
              memo :: Int -> Nimber
              memo = (generate (getMax (fold (Max . inject) p) + 1) (nimber' . justOut) !)
              -- There are at least three other implementations:
              -- Using MemoTrie:
              -- MemoTrie.memo (nimber' . justOut)
              -- Using MemoCombinators:
              -- MemoCombinators.integral (nimber' . justOut)
              -- MemoCombinators.unsafeArrayRange (0, getMax (fold (Max . inject) p)) (nimber' . justOut)
              -- On a test using Kayles, the vector implemenation is fastest. The array implementation is slghtly slower.
              -- Both trie implementations are significantly slower.
