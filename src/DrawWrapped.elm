module DrawWrapped exposing (drawWrapped)

import Collage exposing (Form, group, move)
import Bounds exposing (..)

drawWrapped : Form -> Form
drawWrapped form =
  group
    [ form
    , form |> move (-width, 0)
    , form |> move (width, 0)
    , form |> move (0, -height)
    , form |> move (0, height)
    , form |> move (-width, -height)
    , form |> move (width, -height)
    , form |> move (-width, height)
    , form |> move (width, height)
    ]
