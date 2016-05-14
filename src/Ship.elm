module Ship exposing (front, draw)

import Collage exposing (Form, group, polygon, outlined, defaultLine)
import Color exposing (..)
import Vector exposing (..)

front : Vector -> Float -> Vector
front position direction = add position (rotate direction (0, 12))

left : Vector -> Float -> Vector
left position direction = add position (rotate direction (-6, -6))

right : Vector -> Float -> Vector
right position direction = add position (rotate direction (6, -6))

draw : Vector -> Float -> Form
draw position direction =
  let
    front' = front position direction
    left' = left position direction
    right' = right position direction
  in
    polygon [front', left', right']
    |> outlined { defaultLine | color = white }
