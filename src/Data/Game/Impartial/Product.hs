{-# LANGUAGE FlexibleInstances, TypeFamilies, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables #-}

{-| This module is based on the Tartan theorem.

A product position is a produt of independent base positions
-}
module Data.Game.Impartial.Product (ProductPosition(..)) where

import Data.Game.Impartial.Impartial (Positional(..), HasNimber(..), Position(..), Normal(..))
import Data.Game.Impartial.Base (BasePosition(..))
import Data.Game.Impartial.Simple (Simple(..))
import Data.Game.Impartial.Injection (Injection(..))
import Data.Game.Impartial.Sum (SumPosition(..))
import Data.Monoid (Sum(..))
import Data.Coerce (coerce)

{-|
A product position is a produt of independent base positions
@p@ and @q@ must themselves already be base positions.
This interface is used when the nimber is calculated by nimber product.
In general a @BasePosition (a, b)@ doesn't have to have this property.
-}
newtype ProductPosition p q = ProductPosition (p, q)

instance Injection (BasePosition (p, q)) (ProductPosition p q) where
  inject = coerce
  out = Just . coerce

instance Positional (ProductPosition p q) where
  type Base (ProductPosition p q) = BasePosition (p, q)
  fold f = f . coerce
  cata f = f . coerce

instance (Position p, Position q, Next p ~  p, Next q ~  q) => Position (ProductPosition p q) where
    type Next (ProductPosition p q) = SumPosition (ProductPosition p q)
    next (ProductPosition (p, q)) =
        let q's = next q
            p's = next p in
            coerce [ [ (p, q'), (p', q), (p', q')] | p' <- p's, q' <- q's]

{-| A simple product is a sum of produts of independent positions. This interface doesn't depend on the 'Position' interface. -}
instance (HasNimber (Normal p), HasNimber (Normal q)) => HasNimber (Normal (Simple (ProductPosition p q))) where
  nimber (Normal (Simple (ProductPosition (p, q)))) = nimber (coerce [p] :: Normal (Simple (SumPosition p))) *
                                                                      nimber (coerce [q] :: Normal (Simple (SumPosition q)))
