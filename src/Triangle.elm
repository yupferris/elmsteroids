module Triangle exposing (Triangle, segments, liesInside)

import Vector exposing (..)
import Segment exposing (..)

type alias Triangle =
  { a : Vector
  , b : Vector
  , c : Vector
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
