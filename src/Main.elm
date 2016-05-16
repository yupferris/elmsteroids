import Html exposing (..)
import Html.App
import Random exposing (..)
import Time exposing (..)
import AnimationFrame exposing (..)
import Keyboard exposing (..)
import Collage exposing (..)
import Element
import Color exposing (..)
import Bounds exposing (..)
import Player exposing (Player)
import Asteroids exposing (Asteroid)
import Bullets exposing (Bullet)
import KeyStates exposing (KeyStates)
import Collisions exposing (..)

main =
  Html.App.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model =
  { player : Player
  , asteroids : List Asteroid
  , bullets : List Bullet
  , keys : KeyStates
  , randomSeed : Seed
  }

init : (Model, Cmd Msg)
init =
  let
    -- TODO: Different seed each time
    (asteroids, randomSeed) = initialSeed 12345 |> Asteroids.init

  in
    ({ player =
         { position = (0, 0)
         , velocity = (0, 0)
         , rotation = 0
         }
     , asteroids = asteroids
     , bullets = []
     , keys =
         { left = False
         , right = False
         , up = False
         , down = False
         , spaceTapped = False
         }
     , randomSeed = randomSeed
     }, Cmd.none)

type Msg
  = Tick Float -- Time value is always in seconds
  | KeyPressed KeyCode
  | KeyReleased KeyCode

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (case msg of
     Tick timeDelta ->
       let
         asteroids = Asteroids.tick timeDelta model.asteroids
         bullets = Bullets.tick timeDelta model.keys model.player model.bullets

         (asteroids', bullets') = collide asteroids bullets
       in
         { model
         | player = Player.tick timeDelta model.keys model.player
         , asteroids = asteroids'
         , bullets = bullets'
         , keys = KeyStates.tick model.keys
         }

     KeyPressed key -> { model | keys = KeyStates.pressed key model.keys }
     KeyReleased key -> { model | keys = KeyStates.released key model.keys }
  , Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ diffs (inSeconds >> Tick)

    , downs KeyPressed
    , ups KeyReleased
    ]

view : Model -> Html Msg
view model =
  collage
    (floor width) (floor height)
    [ rect width height |> filled black
    , Asteroids.draw model.asteroids
    , Player.draw model.player
    , Bullets.draw model.bullets
    ]
  |> Element.toHtml
