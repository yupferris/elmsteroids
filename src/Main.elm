import Html.App
import Time exposing (..)
import AnimationFrame exposing (..)
import Keyboard exposing (..)
import Collage exposing (..)
import Element
import Color exposing (..)
import Bounds exposing (..)
import Player exposing (Player)
import Bullets exposing (Bullet)
import KeyStates exposing (KeyStates)

main =
  Html.App.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model =
  { player : Player
  , bullets : List Bullet
  , keys : KeyStates
  }

init =
  ({ player =
     { position = (0, 0)
     , velocity = (0, 0)
     , direction = 0
     }
   , bullets = []
   , keys =
     { left = False
     , right = False
     , up = False
     , down = False
     , spaceTapped = False
     }
   }, Cmd.none)

type Msg
  = Tick Float -- Time value is always in seconds
  | KeyPressed KeyCode
  | KeyReleased KeyCode

update msg model =
  (case msg of
     Tick timeDelta ->
       { model
       | player = Player.tick timeDelta model.keys model.player
       , bullets = Bullets.tick timeDelta model.keys model.player model.bullets
       , keys = KeyStates.tick model.keys
       }

     KeyPressed key -> { model | keys = KeyStates.pressed key model.keys }
     KeyReleased key -> { model | keys = KeyStates.released key model.keys }
  , Cmd.none)

subscriptions _ =
  Sub.batch
    [ diffs (inSeconds >> Tick)

    , downs KeyPressed
    , ups KeyReleased
    ]

view model =
  collage
    width height
    [ rect width height |> filled black
    , Player.draw model.player
    , Bullets.draw model.bullets
    ]
  |> Element.toHtml
