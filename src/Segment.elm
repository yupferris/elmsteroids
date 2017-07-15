module Segment exposing (Segment, center, intersect, wrap)

import Vector exposing (..)
import Wrap


type alias Segment =
    { a : Vector
    , b : Vector
    }


offset : Vector -> Segment -> Segment
offset o segment =
    let
        o_ =
            add o
    in
        { a = o_ segment.a
        , b = o_ segment.b
        }


center : Segment -> Vector
center segment =
    add segment.a segment.b
        |> divS 2


intersect : Segment -> Segment -> Bool
intersect a b =
    let
        r =
            sub a.b a.a

        s =
            sub b.b b.a

        rxs =
            cross r s

        bma =
            sub b.a a.a

        qpxr =
            cross bma r
    in
        if rxs == 0.0 && qpxr == 0.0 then
            -- Segments are collinear
            --  For the sake of brevity, we can simply ignore
            --  this case; realistically the chances of this
            --  happening _and_ the lines actually colliding
            --  should be very low in practice.
            False
        else if rxs == 0.0 && qpxr /= 0.0 then
            -- Segments are parallel and non-intersecting
            False
        else
            -- Segments' lines intersect, but the segments
            --  might not
            let
                t =
                    cross bma s / rxs

                u =
                    cross bma r / rxs
            in
                rxs /= 0.0 && (0 <= t && t <= 1) && (0 <= u && u <= 1)


wrap : Segment -> List Segment
wrap =
    Wrap.wrap
        (\bound segment -> Tuple.first segment.a < bound || Tuple.first segment.b < bound)
        (\bound segment -> Tuple.first segment.a > bound || Tuple.first segment.b > bound)
        (\bound segment -> Tuple.second segment.a > bound || Tuple.second segment.b > bound)
        (\bound segment -> Tuple.second segment.a < bound || Tuple.second segment.b < bound)
        offset
