module Canbando.Capability.IdSupply (class IdSupply, genId, hashId) where

import Prelude

import Control.Monad.Reader.Class (class MonadAsk, asks)
import Data.BigInt (BigInt, fromInt, fromString, toBase, xor)
import Data.Char (toCharCode)
import Data.Maybe (fromMaybe)
import Data.Monoid (power)
import Data.String (codePointFromChar, length, singleton, toUpper)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref, modify)
import Halogen (HalogenM, lift)


class (Monad m) <= IdSupply m where
  genId :: Char -> m String

instance idSupplyHalogenM ::
  IdSupply m => IdSupply (HalogenM state action slots output m) where
  genId = lift <<< genId


hashId ::
  forall m r.
  MonadAsk { idSupply :: Ref Int | r } m =>
  MonadEffect m =>
  Char -> m String
hashId pfx = do
  ref <- asks _.idSupply
  id <- liftEffect $ modify (_ + 1) ref
  let raw = sequenceRNG (toCharCode pfx) id
      pad = power "0" (8 - length raw)
  pure $ singleton (codePointFromChar pfx) <> pad <> raw



prime :: BigInt
prime = fromMaybe zero $ fromString "4294967291"

permuteQPR :: BigInt -> BigInt
permuteQPR n =
    if n >= prime
    then n    -- The 5 integers out of range are mapped to themselves.
    else
      let residue = (n * n) `mod` prime
      in if n <= prime / fromInt 2 then residue else prime - residue

sequenceRNG :: Int -> Int -> String
sequenceRNG offset n =
  toUpper $ toBase 16 $
  permuteQPR (permuteQPR (fromInt n) + fromInt offset) `xor` fromInt 0x5BF03635
