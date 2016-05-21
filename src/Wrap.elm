module Wrap exposing (wrap)

import List exposing (..)
import Vector exposing (..)
import Bounds exposing (..)

type alias WrapPred a = Float -> a -> Bool

wrap : WrapPred a -> WrapPred a -> WrapPred a -> WrapPred a -> (Vector -> a -> a) -> a -> List a
wrap leftPred rightPred topPred bottomPred f item =
  [item]
    |> wrap' (leftPred left) (f (width, 0))
    |> wrap' (rightPred right) (f (-width, 0))
    |> wrap' (topPred top) (f (0, -height))
    |> wrap' (bottomPred bottom) (f (0, height))

wrap' : (a -> Bool) -> (a -> a) -> List a -> List a
wrap' pred f items =
  if any pred items then items ++ map f items
  else items
