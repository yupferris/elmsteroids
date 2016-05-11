import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)

main =
  App.program
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

update msg model =
  (model, Cmd.none)

view model =
  svg [ viewBox "0 0 100 100" ]
    [ rect [ fill "#000", x "0", y "0", width "100", height "100" ] []
    , polyline [ fill "none", stroke "#fff", points "20,20 80,20 50,80 20,20" ] []
    ]
