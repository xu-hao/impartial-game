{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, TypeFamilies, TypeApplications, ScopedTypeVariables #-}

{-|
A base position is a position that cannot be further decomposed.
This module is to provide default instances for base positions.
For example, to define a base position represented by type @A@, all we need to do is define the 'Position' instance,

> instance Position (BasePosition A) where
>     ...

and making sure that

> Base (Next (BasePosition A)) == BasePosition A

which is guaranteed by 'Simple', 'SumPosition', and 'ProductPosition'.

-}
module Data.Game.Impartial.Base (BasePosition(..)) where


import Data.Game.Impartial.Injection (Injection(..))
import Data.Game.Impartial.Impartial (Positional(..), HasNimber(..), NormalPosition(..), Normal(..))
import Data.Coerce (coerce)

{-|
A base position is a position that cannot be further decomposed.
-}
newtype BasePosition a = BasePosition a

instance Injection (BasePosition a) (BasePosition a) where

instance Injection a Int => Injection (BasePosition a) Int where
    inject = inject @a . coerce
    out = coerce . out @a

instance Injection a Int => Positional (BasePosition a) where
    type Base (BasePosition a) = (BasePosition a)
    fold = (. coerce)
    cata = (. coerce)

instance HasNimber (NormalPosition (BasePosition a)) => HasNimber (Normal (BasePosition a)) where
    nimber = nimber . (coerce :: Normal (BasePosition a) -> NormalPosition (BasePosition a))
