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
  | Tick Time
  | KeyPressed KeyCode
  | KeyReleased KeyCode

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (case model of
     Uninitialized ->
       case msg of
         Init time -> Game (initGame time)
         _ -> model

     Game gameState ->
       Game
         (case msg of
            Init _ -> gameState
            Tick timeDelta -> tickGame (inSeconds timeDelta) gameState

            KeyPressed key -> { gameState | keys = KeyStates.pressed key gameState.keys }
            KeyReleased key -> { gameState | keys = KeyStates.released key gameState.keys }
         )

  , Cmd.none)

initGame : Time -> GameState
initGame time =
  let
    ms = inMilliseconds time |> floor
    (asteroids, randomSeed) = initialSeed ms |> Asteroids.init

  in
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

-- Time value is always in seconds
tickGame : Float -> GameState -> GameState
tickGame timeDelta gameState =
  let
    asteroids = Asteroids.tick timeDelta gameState.asteroids
    bullets = Bullets.tick timeDelta gameState.keys gameState.player gameState.bullets

    ((asteroids', bullets'), randomSeed) = collide asteroids bullets gameState.randomSeed
  in
    { gameState
      | player = Player.tick timeDelta gameState.keys gameState.player
      , asteroids = asteroids'
      , bullets = bullets'
      , keys = KeyStates.tick gameState.keys
      , randomSeed = randomSeed
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    Uninitialized -> times Init
    Game _ ->
      Sub.batch
        [ diffs Tick

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
