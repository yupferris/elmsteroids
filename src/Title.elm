module Title exposing (draw)

import Collage exposing (Form, group, moveY)
import DefaultText exposing (..)
import Bounds exposing (..)

draw : Form
draw =
  group
    [ defaultText 40 "elmsteroids" |> moveY 50
    , defaultText 16 "github.com/yupferris // 2016" |> moveY -30
    , defaultText 14 "press enter/return to begin" |> moveY -50
    ]
