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
  { position : Point
  , angle : Float
  }

type alias Point = (Float, Float)

add x y =
  let
    (xx, xy) = x
    (yx, yy) = y
  in (xx + yx, xy + yy)

rotate angle point =
  let
    (x, y) = point
    c = cos angle
    s = sin angle
  in (x * c + y * s, y * c - x * s)

type alias KeyStates =
  { left : Bool
  , right : Bool
  , up : Bool
  , down : Bool
  }

init =
  ({ player =
     { position = (0, 0)
     , angle = 0
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
    delta = 0.03
    leftDelta = if keys.left == True then -delta else 0
    rightDelta = if keys.right == True then delta else 0
    angleDelta = leftDelta + rightDelta
    -- upDelta = if keys.up == True then 1 else 0
    -- downDelta = if keys.down == True then -1 else 0
    -- yDelta = upDelta + downDelta
    angle = player.angle + angleDelta
  in
    { player | angle = angle }

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
                 if key == left then LeftPressed
                 else if key == right then RightPressed
                 else if key == up then UpPressed
                 else if key == down then DownPressed
                 else None)
      , ups (\key ->
               if key == left then LeftReleased
               else if key == right then RightReleased
               else if key == up then UpReleased
               else if key == down then DownReleased
               else None)
      ]

view model =
  let
    width = 500
    height = 500

  in
    collage
      width height
      [ rect width height |> filled Color.black
      , drawPlayer model.player
      ]
    |> Element.toHtml

drawPlayer player =
  -- circle 5 |> filled Color.white |> move player.position
  let
    rotate' = rotate player.angle
    front = add player.position (rotate' (0, 12))
    left = add player.position (rotate' (-6, -6))
    right = add player.position (rotate' (6, -6))
  in
    path [front, left, right, front]
    |> traced { defaultLine | color = Color.white }
