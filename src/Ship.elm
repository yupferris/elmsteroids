module Ship exposing (front, left, right, draw)

import Collage exposing (group, path, traced, defaultLine)
import Color exposing (..)
import Vector exposing (..)

front position direction = add position (rotate direction (0, 12))
left position direction = add position (rotate direction (-6, -6))
right position direction = add position (rotate direction (6, -6))

draw position direction =
  let
    front' = front position direction
    left' = left position direction
    right' = right position direction
  in
    path [front', left', right', front']
    |> traced { defaultLine | color = white }
