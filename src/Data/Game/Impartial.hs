{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, TypeApplications #-}

{-| Impartial games, calculating their nimbers, or Grundy numbers. Main module

    See <https://en.wikipedia.org/wiki/impartial_game>.
-}

module Data.Game.Impartial (
  module Data.Game.Impartial.Impartial, module Data.Game.Impartial.Injection, module Data.Game.Impartial.Simple,
  module Data.Game.Impartial.Product, module Data.Game.Impartial.Base, module Data.Game.Impartial.Sum) where

import Data.Game.Impartial.Impartial
import Data.Game.Impartial.Product
import Data.Game.Impartial.Injection
import Data.Game.Impartial.Base
import Data.Game.Impartial.Sum
import Data.Game.Impartial.Simple
