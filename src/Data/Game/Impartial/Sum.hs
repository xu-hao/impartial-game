{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, TypeApplications #-}

{-| A sum position is a sum of independent positions -}
module Data.Game.Impartial.Sum (SumPosition(..)) where

import Data.Monoid (Sum(..))
import Data.Coerce (coerce, Coercible)
import Data.Game.Impartial.Injection (Injection(..))
import Data.Game.Impartial.Impartial (Positional(..), Position(..), HasNimber(..), Normal(..))
import Data.Game.Impartial.Simple (Simple(..))


{-| A sum position is a sum of independent positions -}
newtype SumPosition p = SumPosition [p]

instance Injection b p => Injection b (SumPosition p) where
    inject = coerce . (: []) . inject @b @p
    out (SumPosition [p]) = out p
    out _ = Nothing

instance Positional p => Positional (SumPosition p) where
    type Base (SumPosition p) = Base p
    fold f = foldMap @[] (fold @p f) . coerce
    cata = (getSum .) . fold . (Sum .)

instance (Position p, Coercible (Next p) [p]) => Position (SumPosition p) where
    type Next (SumPosition p) = SumPosition p
    next (SumPosition []) = []
    next (SumPosition (p : ps)) = coerce ([ p' ++ ps | p' <- coerce (next p) ] ++ [ p : ps' | ps' <- coerce (next @(SumPosition p) (coerce ps))])

instance HasNimber (Normal p) => HasNimber (Normal (Simple (SumPosition p))) where
    nimber = sum . map (nimber @(Normal p)) . coerce
