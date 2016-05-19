module Hud exposing (draw)

import Collage exposing (Form, moveY)
import DefaultText exposing (..)
import Bounds exposing (..)

draw : Int -> Int -> Form
draw sector score =
  defaultText 12 ("sector " ++ toString sector ++ " // score: " ++ toString score)
    |> moveY (top - 10)
