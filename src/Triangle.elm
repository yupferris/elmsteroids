module Triangle exposing (Triangle, segments, liesInside, wrap)

import List exposing (..)
import Bounds exposing (..)
import Vector exposing (..)
import Segment exposing (..)

type alias Triangle =
  { a : Vector
  , b : Vector
  , c : Vector
  }

offset : Vector -> Triangle -> Triangle
offset o triangle =
  let o' = add o
  in
    { a = o' triangle.a
    , b = o' triangle.b
    , c = o' triangle.c
    }

segments : Triangle -> List Segment
segments triangle =
  [ { a = triangle.a, b = triangle.b }
  , { a = triangle.b, b = triangle.c }
  , { a = triangle.c, b = triangle.a }
  ]

liesInside : Vector -> Triangle -> Bool
liesInside point triangle =
  let
    a = triangle.a
    b = triangle.b
    c = triangle.c

    v0 = c `sub` a
    v1 = b `sub` a
    v2 = point `sub` a

    dot00 = dot v0 v0
    dot01 = dot v0 v1
    dot02 = dot v0 v2
    dot11 = dot v1 v1
    dot12 = dot v1 v2

    denom = dot00 * dot11 - dot01 ^ 2

    u = (dot11 * dot02 - dot01 * dot12) / denom
    v = (dot00 * dot12 - dot01 * dot02) / denom
  in u >= 0 && v >= 0 && u + v < 1

wrap : Triangle -> List Triangle
wrap triangle =
  [triangle]
    |> wrap' (\triangle -> fst triangle.a < left || fst triangle.b < left || fst triangle.c < left) (offset (width, 0))
    |> wrap' (\triangle -> fst triangle.a > right || fst triangle.b > right || fst triangle.c > right) (offset (-width, 0))
    |> wrap' (\triangle -> snd triangle.a > top || snd triangle.b > top || snd triangle.c > top) (offset (0, -height))
    |> wrap' (\triangle -> snd triangle.a < bottom || snd triangle.b < bottom || snd triangle.c < bottom) (offset (0, height))

wrap' : (Triangle -> Bool) -> (Triangle -> Triangle) -> List Triangle -> List Triangle
wrap' f g triangles =
  if any f triangles then triangles ++ map g triangles
  else triangles
