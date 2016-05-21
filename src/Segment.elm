module Segment exposing (Segment, center, intersect, wrap)

import List exposing (..)
import Vector exposing (..)
import Bounds exposing (..)

type alias Segment =
  { a : Vector
  , b : Vector
  }

offset : Vector -> Segment -> Segment
offset o segment =
  let o' = add o
  in
    { a = o' segment.a
    , b = o' segment.b
    }

center : Segment -> Vector
center segment =
  add segment.a segment.b
    |> divS 2

intersect : Segment -> Segment -> Bool
intersect a b =
  let
    r = a.b `sub` a.a
    s = b.b `sub` b.a
    rxs = cross r s
    bma = b.a `sub` a.a
    qpxr = cross bma r
  in
    if rxs == 0.0 && qpxr == 0.0 then
      -- Segments are collinear
      --  For the sake of brevity, we can simply ignore
      --  this case; realistically the chances of this
      --  happening _and_ the lines actually colliding
      --  should be very low in practice.
      False
    else
      if rxs == 0.0 && qpxr /= 0.0 then
        -- Segments are parallel and non-intersecting
        False
      else
        -- Segments' lines intersect, but the segments
        --  might not
        let
          t = cross bma s / rxs
          u = cross bma r / rxs
        in
          rxs /= 0.0 && (0 <= t && t <= 1) && (0 <= u && u <= 1)

wrap : Segment -> List Segment
wrap segment =
  [segment]
    |> wrap' (\segment -> fst segment.a < left || fst segment.b < left) (offset (width, 0))
    |> wrap' (\segment -> fst segment.a > right || fst segment.b > right) (offset (-width, 0))
    |> wrap' (\segment -> snd segment.a > top || snd segment.b > top) (offset (0, -height))
    |> wrap' (\segment -> snd segment.a < bottom || snd segment.b < bottom) (offset (0, height))

wrap' : (Segment -> Bool) -> (Segment -> Segment) -> List Segment -> List Segment
wrap' f g segments =
  if any f segments then segments ++ map g segments
  else segments
