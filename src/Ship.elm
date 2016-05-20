module Ship exposing (front, triangle, draw)

import Collage exposing (Form, group, polygon, filled, outlined, defaultLine)
import Color exposing (..)
import Vector exposing (..)
import Triangle exposing (..)

front : Vector -> Float -> Vector
front position rotation = add position (rotate rotation (0, 12))

left : Vector -> Float -> Vector
left position rotation = add position (rotate rotation (-6, -6))

right : Vector -> Float -> Vector
right position rotation = add position (rotate rotation (6, -6))

triangle : Vector -> Float -> Triangle
triangle position rotation =
  { a = front position rotation
  , b = left position rotation
  , c = right position rotation
  }

draw : Vector -> Float -> Form
draw position rotation =
  let
    front' = front position rotation
    left' = left position rotation
    right' = right position rotation

    form = polygon [front', left', right']
  in
    group
      [ form |> filled black
      , form |> outlined { defaultLine | color = white }
      ]
