module Title exposing (draw)

import Collage exposing (Form, group, moveY)
import DefaultText exposing (..)
import Bounds exposing (..)

draw : Form
draw =
  group
    [ defaultText 40 "elmsteroids" |> moveY 70
    , defaultText 16 "github.com/yupferris // 2016" |> moveY -50
    ]
