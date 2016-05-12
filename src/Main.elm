import Html.App
import Color exposing (Color)
import Collage exposing (..)
import Element exposing (..)

main =
  Html.App.program
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }

type alias Position =
  { x : Float
  , y : Float
  }

type alias Player =
  { position: Position
  }

type alias Model =
  { player : Player
  }

init = (Model (Player (Position 0 0)), Cmd.none)

update _ model =
  (model, Cmd.none)

view _ =
  collage
    100 100
    [ rect 100 100 |> filled Color.black
    , circle 5 |> filled Color.white
    ]
    |> Element.toHtml
