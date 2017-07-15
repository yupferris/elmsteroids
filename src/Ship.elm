module Ship exposing (front, triangle, draw)

import List exposing (..)
import Collage exposing (Form, group, polygon, filled, outlined, defaultLine)
import Color exposing (..)
import Vector exposing (Vector, add, rotate)
import Triangle exposing (..)


front : Vector -> Float -> Vector
front position rotation =
    add position (rotate rotation ( 0, 12 ))


left : Vector -> Float -> Vector
left position rotation =
    add position (rotate rotation ( -6, -6 ))


right : Vector -> Float -> Vector
right position rotation =
    add position (rotate rotation ( 6, -6 ))


triangle : Vector -> Float -> Triangle
triangle position rotation =
    { a = front position rotation
    , b = left position rotation
    , c = right position rotation
    }


draw : Vector -> Float -> Form
draw position rotation =
    triangle position rotation
        |> wrap
        |> map
            (\t ->
                let
                    form =
                        polygon [ t.a, t.b, t.c ]
                in
                    group
                        [ form |> filled black
                        , form |> outlined { defaultLine | color = white }
                        ]
            )
        |> group
