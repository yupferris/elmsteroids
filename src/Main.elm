import Html exposing (..)
import Html.App
import Random exposing (..)
import Time exposing (..)
import AnimationFrame exposing (..)
import Keyboard exposing (..)
import Collage exposing (collage, rect, filled)
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

type Model
  = Uninitialized
  | Game GameState

type alias GameState =
  { player : Player
  , asteroids : List Asteroid
  , bullets : List Bullet
  , keys : KeyStates
  , randomSeed : Seed
  }

init : (Model, Cmd Msg)
init = (Uninitialized, Cmd.none)

type Msg
  = Init Time
  | Tick Float -- Time value is always in seconds
  | KeyPressed KeyCode
  | KeyReleased KeyCode

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (case model of
     Uninitialized ->
       case msg of
         Init time ->
           let
             ms = inMilliseconds time |> floor
             (asteroids, randomSeed) = initialSeed ms |> Asteroids.init

           in
             Game
               { player =
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
               }
         _ -> model

     Game gameState ->
       case msg of
         Init _ -> model
         Tick timeDelta ->
           let
             asteroids = Asteroids.tick timeDelta gameState.asteroids
             bullets = Bullets.tick timeDelta gameState.keys gameState.player gameState.bullets

             ((asteroids', bullets'), randomSeed') = collide asteroids bullets gameState.randomSeed
           in
             Game
               { gameState
               | player = Player.tick timeDelta gameState.keys gameState.player
               , asteroids = asteroids'
               , bullets = bullets'
               , keys = KeyStates.tick gameState.keys
               , randomSeed = randomSeed'
               }

         KeyPressed key -> Game { gameState | keys = KeyStates.pressed key gameState.keys }
         KeyReleased key -> Game { gameState | keys = KeyStates.released key gameState.keys }

  , Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    Uninitialized -> times Init
    Game _ ->
      Sub.batch
        [ diffs (inSeconds >> Tick)

        , downs KeyPressed
        , ups KeyReleased
        ]

view : Model -> Html Msg
view model =
  case model of
    Uninitialized -> text "Initializing..."
    Game gameState ->
      collage
        (floor width) (floor height)
        [ rect width height |> filled black
        , Asteroids.draw gameState.asteroids
        , Player.draw gameState.player
        , Bullets.draw gameState.bullets
        ]
      |> Element.toHtml
