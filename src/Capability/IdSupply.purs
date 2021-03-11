module Canbando.Capability.IdSupply (class IdSupply, genId, hashId) where

import Prelude

import Control.Monad.Reader.Class (class MonadAsk, asks)
import Data.BigInt (fromInt, toBase)
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
  id <- asks _.idSupply >>= liftEffect <<< modify (_ + 1)
  let raw = toUpper $ toBase 16 $ fromInt id
      pad = power "0" (8 - length raw)
  pure $ singleton (codePointFromChar pfx) <> pad <> raw
