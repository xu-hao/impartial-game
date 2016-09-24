{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, TypeFamilies, TypeApplications, ScopedTypeVariables #-}

{-\
A wrapper for calculating the nimber of the current postion without using the 'next' function

-}
module Data.Game.Impartial.Simple (Simple(..)) where


import Data.Game.Impartial.Injection (Injection(..))
import Data.Game.Impartial.Impartial(Positional(..))
import Data.Coerce (coerce)

{-|
  A wrapper for calculating the nimber of the current postion without using the 'next' function
-}
newtype Simple p = Simple p

instance (b ~ Base a, Positional a) => Injection b (Simple a) where
    inject = (coerce :: a -> Simple a) . inject
    out = out . (coerce :: Simple a -> a)

instance Positional a => Positional (Simple a) where
    type Base (Simple a) = Base a
    fold f = fold f . (coerce :: Simple a -> a)
    cata f = cata f . (coerce :: Simple a -> a)
