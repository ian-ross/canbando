module Canbando.Model.Board where

import Data.Array.NonEmpty (NonEmptyArray)

import Canbando.Model.List (List)


type Id = String

type BoardRep row =
  ( id :: Id
  , name :: String
  , lists :: NonEmptyArray List
  | row )

type Board = { | BoardRep () }
