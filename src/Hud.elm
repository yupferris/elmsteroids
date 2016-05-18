module Hud exposing (draw)

import Collage exposing (Form, moveY)
import DefaultText exposing (..)
import Bounds exposing (..)

draw : Int -> Form
draw score =
  defaultText 12 ("score: " ++ toString score)
    |> moveY (top - 10)
