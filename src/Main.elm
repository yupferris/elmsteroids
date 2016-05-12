import Html.App
import AnimationFrame exposing (..)
import Keyboard exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)

main =
  Html.App.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model =
  { player : Player
  , keys : KeyStates
  }

type alias Player =
  { position : Position
  }

type alias Position = (Float, Float)

type alias KeyStates =
  { left : Bool
  , right : Bool
  , up : Bool
  , down : Bool
  }

init =
  ({ player =
     { position = (0, 0)
     }
   , keys =
     { left = False
     , right = False
     , up = False
     , down = False
     }
   }, Cmd.none)

type Msg
  = None
  | Tick
  | LeftPressed | LeftReleased
  | RightPressed | RightReleased
  | UpPressed | UpReleased
  | DownPressed | DownReleased

update msg model =
  let model' =
    case msg of
      None -> model

      Tick -> { model | player = updatePlayer model.player model.keys }

      -- { model.keys | ... } didn't work here; compiler didn't understand the left value could be the result of a function application
      LeftPressed -> { model | keys = let keys = model.keys in { keys | left = True } }
      LeftReleased -> { model | keys = let keys = model.keys in { keys | left = False } }
      RightPressed -> { model | keys = let keys = model.keys in { keys | right = True } }
      RightReleased -> { model | keys = let keys = model.keys in { keys | right = False } }
      UpPressed -> { model | keys = let keys = model.keys in { keys | up = True } }
      UpReleased -> { model | keys = let keys = model.keys in { keys | up = False } }
      DownPressed -> { model | keys = let keys = model.keys in { keys | down = True } }
      DownReleased -> { model | keys = let keys = model.keys in { keys | down = False } }
  in (model', Cmd.none)

updatePlayer player keys =
  let
    (x, y) = player.position
    leftDelta = if keys.left == True then -1 else 0
    rightDelta = if keys.right == True then 1 else 0
    xDelta = leftDelta + rightDelta
    upDelta = if keys.up == True then 1 else 0
    downDelta = if keys.down == True then -1 else 0
    yDelta = upDelta + downDelta
    x' = x + xDelta
    y' = y + yDelta
  in
    { position = (x', y') }

subscriptions _ =
  let
    left = 37
    right = 39
    up = 38
    down = 40
    space = 32

  in
    Sub.batch
      [ times (\_ -> Tick)
      , downs (\key ->
                 -- For some reason case didn't work here
                 -- The compiler thought `left` and `right` were the same patterns (?)
                 if key == left then
                   LeftPressed
                 else if key == right then
                   RightPressed
                 else if key == up then
                   UpPressed
                 else if key == down then
                   DownPressed
                 else
                   None)
      , ups (\key ->
               if key == left then
                 LeftReleased
               else if key == right then
                 RightReleased
               else if key == up then
                 UpReleased
               else if key == down then
                 DownReleased
               else
                 None)
      ]

view model =
  let
    width = 500
    height = 500

  in
    collage
      width height
      [ rect width height |> filled Color.black
      , circle 5 |> filled Color.white |> move model.player.position
      ]
    |> Element.toHtml
