module Segment exposing (Segment, center, intersect, wrap)

import List exposing (..)
import Vector exposing (..)
import Bounds exposing (..)
import Wrap

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
wrap =
  Wrap.wrap
        (\bound segment -> fst segment.a < bound || fst segment.b < bound)
        (\bound segment -> fst segment.a > bound || fst segment.b > bound)
        (\bound segment -> snd segment.a > bound || snd segment.b > bound)
        (\bound segment -> snd segment.a < bound || snd segment.b < bound)
        offset
