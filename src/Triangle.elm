module Triangle exposing (Triangle, segments, liesInside, wrap)

import Vector exposing (..)
import Segment exposing (..)
import Wrap


type alias Triangle =
    { a : Vector
    , b : Vector
    , c : Vector
    }


offset : Vector -> Triangle -> Triangle
offset o triangle =
    let
        o_ =
            add o
    in
        { a = o_ triangle.a
        , b = o_ triangle.b
        , c = o_ triangle.c
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
        a =
            triangle.a

        b =
            triangle.b

        c =
            triangle.c

        v0 =
            sub c a

        v1 =
            sub b a

        v2 =
            sub point a

        dot00 =
            dot v0 v0

        dot01 =
            dot v0 v1

        dot02 =
            dot v0 v2

        dot11 =
            dot v1 v1

        dot12 =
            dot v1 v2

        denom =
            dot00 * dot11 - dot01 ^ 2

        u =
            (dot11 * dot02 - dot01 * dot12) / denom

        v =
            (dot00 * dot12 - dot01 * dot02) / denom
    in
        u >= 0 && v >= 0 && u + v < 1


anyPoints : (Vector -> Bool) -> Triangle -> Bool
anyPoints f triangle =
    f triangle.a || f triangle.b || f triangle.c


wrap : Triangle -> List Triangle
wrap =
    Wrap.wrap
        (\bound -> anyPoints (\( x, _ ) -> x < bound))
        (\bound -> anyPoints (\( x, _ ) -> x > bound))
        (\bound -> anyPoints (\( _, y ) -> y > bound))
        (\bound -> anyPoints (\( _, y ) -> y < bound))
        offset
