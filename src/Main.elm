import Html exposing (Html)
import Html.App
import Random exposing (..)
import Time exposing (..)
import AnimationFrame exposing (..)
import Keyboard exposing (..)
import Text exposing (fromString, style, link)
import Collage exposing (Form, collage, group, rect, filled, text, moveY)
import Element
import Color exposing (..)
import State exposing (..)
import DefaultText exposing (..)
import Bounds exposing (..)
import Stars exposing (Star)
import Player exposing (Player)
import Asteroids exposing (Asteroid)
import Bullets exposing (Bullet)
import SegmentParticles exposing (SegmentParticle)
import KeyStates exposing (KeyStates)
import Collisions exposing (..)
import Hud

main =
  Html.App.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Model
  = Uninitialized
  | Title TitleState
  | PreGame PreGameState
  | Game GameState

type alias TitleState =
  { stars : List Star
  , asteroids : List Asteroid
  , randomSeed : Seed
  }

type alias PreGameState =
  { score : Int
  , stars : List Star
  , asteroids : List Asteroid
  , randomSeed : Seed
  }

type alias GameState =
  { score : Int
  , stars : List Star
  , player : Player
  , asteroids : List Asteroid
  , bullets : List Bullet
  , segmentParticles : List SegmentParticle
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
         Init time -> Title (initTitle time)
         _ -> model

     Title titleState ->
       (case msg of
          Tick timeDelta -> Title (tickTitle (inSeconds timeDelta) titleState)

          KeyPressed key ->
            let enter = 13
            in
              if key == enter then
                PreGame (initPreGame 0 titleState.stars titleState.asteroids titleState.randomSeed)
              else model

          _ -> model)

     PreGame preGameState ->
       (case msg of
          Tick timeDelta -> PreGame (tickPreGame (inSeconds timeDelta) preGameState)

          KeyPressed key ->
            let enter = 13
            in
              if key == enter then
                Game (initGame preGameState.score preGameState.stars preGameState.asteroids preGameState.randomSeed)
              else model

          _ -> model)

     Game gameState ->
       Game
         (case msg of
            Init _ -> gameState
            Tick timeDelta -> tickGame (inSeconds timeDelta) gameState

            KeyPressed key -> { gameState | keys = KeyStates.pressed key gameState.keys }
            KeyReleased key -> { gameState | keys = KeyStates.released key gameState.keys }
         )

  , Cmd.none)

initTitle : Time -> TitleState
initTitle time =
  let
    ms = inMilliseconds time |> floor
    ((stars, asteroids), randomSeed) =
      initialSeed ms |> initStarsAndAsteroids

  in
    { stars = stars
    , asteroids = asteroids
    , randomSeed = randomSeed
    }

initStarsAndAsteroids : State Seed (List Star, List Asteroid)
initStarsAndAsteroids =
  Stars.init >>= \stars ->
    Asteroids.init >>= \asteroids ->
      return (stars, asteroids)

tickTitle : Float -> TitleState -> TitleState
tickTitle timeDelta titleState =
  { titleState
    | stars = Stars.tick timeDelta titleState.stars
    , asteroids = Asteroids.tick timeDelta titleState.asteroids
  }

initPreGame : Int -> List Star -> List Asteroid -> Seed -> PreGameState
initPreGame score stars asteroids randomSeed =
  { score = score
  , stars = stars
  , asteroids = asteroids
  , randomSeed = randomSeed
  }

tickPreGame : Float -> PreGameState -> PreGameState
tickPreGame timeDelta preGameState =
  { preGameState
    | stars = Stars.tick timeDelta preGameState.stars
    , asteroids = Asteroids.tick timeDelta preGameState.asteroids
  }

initGame : Int -> List Star -> List Asteroid -> Seed -> GameState
initGame score stars asteroids randomSeed =
  { score = score
  , stars = stars
  , player =
      { position = (0, 0)
      , velocity = (0, 0)
      , rotation = 0
      }
  , asteroids = asteroids
  , bullets = []
  , segmentParticles = []
  , keys =
      { left = False
      , right = False
      , up = False
      , down = False
      , spaceTapped = False
      }
  , randomSeed = randomSeed
  }

tickGame : Float -> GameState -> GameState
tickGame timeDelta gameState =
  let
    asteroids = Asteroids.tick timeDelta gameState.asteroids
    bullets = Bullets.tick timeDelta gameState.keys gameState.player gameState.bullets

    ((asteroids', bullets', segmentParticles, score), randomSeed) = collide asteroids bullets gameState.randomSeed
  in
    { gameState
      | score = gameState.score + score
      , stars = Stars.tick timeDelta gameState.stars
      , player = Player.tick timeDelta gameState.keys gameState.player
      , asteroids = asteroids'
      , bullets = bullets'
      , segmentParticles = SegmentParticles.tick timeDelta gameState.segmentParticles ++ segmentParticles
      , keys = KeyStates.tick gameState.keys
      , randomSeed = randomSeed
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    Uninitialized -> times Init
    Title _ ->
      Sub.batch
           [ diffs Tick

           , downs KeyPressed
           ]
    PreGame _ ->
      Sub.batch
           [ diffs Tick

           , downs KeyPressed
           ]
    Game _ ->
      Sub.batch
           [ diffs Tick

           , downs KeyPressed
           , ups KeyReleased
           ]

view : Model -> Html Msg
view model =
  case model of
    Uninitialized -> Html.text "Initializing..."

    Title titleState ->
      collage
        (floor width) (floor height)
        [ rect width height |> filled black
        , Stars.draw titleState.stars
        , Asteroids.draw titleState.asteroids
        , drawTitle
        ]
        |> Element.toHtml

    PreGame preGameState ->
      collage
        (floor width) (floor height)
        [ rect width height |> filled black
        , Stars.draw preGameState.stars
        , Asteroids.draw preGameState.asteroids
        -- TODO: Pre-game info
        ]
        |> Element.toHtml

    Game gameState ->
      collage
        (floor width) (floor height)
        [ rect width height |> filled black
        , Stars.draw gameState.stars
        , Asteroids.draw gameState.asteroids
        , Player.draw gameState.player
        , Bullets.draw gameState.bullets
        , SegmentParticles.draw gameState.segmentParticles
        , Hud.draw gameState.score
        ]
        |> Element.toHtml

drawTitle : Form
drawTitle =
  group
    [ defaultText 40 "elmsteroids" |> moveY 50
    , defaultText 16 "github.com/yupferris // 2016" |> moveY -30
    , defaultText 14 "press enter/return to begin" |> moveY -50
    ]
