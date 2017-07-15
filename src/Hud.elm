module Hud exposing (livesText, draw)

import Collage exposing (Form, moveY)
import DefaultText exposing (..)
import Bounds exposing (..)


livesText : Int -> String
livesText lives =
    case lives of
        1 ->
            "1 ship remains"

        _ ->
            toString lives ++ " ships remain"


draw : Int -> Int -> Int -> Form
draw sector score lives =
    defaultText 12 ("sector " ++ toString sector ++ " // score: " ++ toString score ++ " // " ++ livesText lives)
        |> moveY (top - 10)
