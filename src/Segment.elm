module Segment exposing (Segment, center, intersect, wrap)

import List exposing (..)
import Vector exposing (..)

type alias Segment =
  { a : Vector
  , b : Vector
  }

offset : Vector -> Segment -> Segment
offset o segment =
  { a = add segment.a o
  , b = add segment.b o
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

wrap : Vector -> Segment -> List Segment
wrap bounds segment =
  let
    (w, h) = bounds
    left = -w / 2
    right = w / 2
    top = h / 2
    bottom = -h / 2
    (xx, xy) = segment.a
    (yx, yy) = segment.b
  in
    [segment]
      |> wrap' (\(x, _) -> x < left) (offset (w, 0))
      |> wrap' (\(x, _) -> x > right) (offset (-w, 0))
      |> wrap' (\(_, y) -> y > top) (offset (0, -h))
      |> wrap' (\(_, y) -> y < bottom) (offset (0, h))

wrap' : (Vector -> Bool) -> (Segment -> Segment) -> List Segment -> List Segment
wrap' f g segments =
  if any (\segment -> f segment.a || f segment.b) segments then
    segments ++ map g segments
  else segments
