{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DefaultSignatures, FlexibleContexts #-}

module Data.Game.Impartial.Injection(Injection(..)) where

import Data.Maybe (fromJust)
import Data.Coerce (Coercible, coerce)

{-|
> forall i p,
> out i == Just p -> inject p == i /\
> out . inject == Just
-}
class Injection a b where
    inject :: a -> b
    out :: b -> Maybe a
    justOut :: b -> a
    justOut = fromJust . out
    default inject :: Coercible a b => a -> b
    inject = coerce
    default out :: Coercible a b => a -> Maybe b
    out = Just . coerce

instance Injection Int Int
instance Injection (p, q) (p, q)


{-|
Fast integer square root described in <https://wiki.haskell.org/Generic_number_type#squareRoot>.
-}
squareRoot :: Int -> Int
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^ 2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r ^ 2 <= n && n < (r+1) ^ 2
   in  head $ dropWhile (not . isRoot) iters

{-|
Implement the "Elegant Pairing Function" described in <http://szudzik.com/ElegantPairing.pdf>.
-}
instance (Injection p Int, Injection q Int) => Injection (p, q) Int where
    inject ( p, q) =
        let a = inject p
            b = inject q in
            if a < b then b ^ 2 + a else b ^ 2 + b + a

    out i =
        let sqrti = squareRoot i
            sqrsqrti = sqrti ^ 2
            diff = i - sqrsqrti in
            if diff < sqrti then (,) <$> out diff <*> out sqrti else (,) <$> out sqrti <*> out (diff - sqrti)
