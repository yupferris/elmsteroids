module Segment exposing (Segment, center)

import Vector exposing (..)

type alias Segment =
  { a : Vector
  , b : Vector
  }

center : Segment -> Vector
center segment =
  add segment.a segment.b
    |> div 2
