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
  { position : Vector
  , velocity: Vector
  , direction : Float
  }

type alias Vector = (Float, Float)

add x y =
  let
    (xx, xy) = x
    (yx, yy) = y
  in (xx + yx, xy + yy)

rotate angle vector =
  let
    (x, y) = vector
    c = cos angle
    s = sin angle
  in (x * c + y * s, y * c - x * s)

wrap bounds vector =
  let
    (w, h) = bounds
    left = -w / 2
    right = w / 2
    top = h / 2
    bottom = -h / 2
    (x, y) = vector
  in
    if x < left then wrap bounds (x + w, y)
    else if x > right then wrap bounds (x - w, y)
    else if y < bottom then wrap bounds (x, y + h)
    else if y > top then wrap bounds (x, y - h)
    else vector

type alias KeyStates =
  { left : Bool
  , right : Bool
  , up : Bool
  , down : Bool
  }

init =
  ({ player =
     { position = (0, 0)
     , velocity = (0, 0)
     , direction = 0
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

width = 500
height = 500
bounds = (width, height)

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
    position =
      add player.position player.velocity
      |> wrap bounds

    accel = 0.01
    upAccel = if keys.up == True then accel else 0
    downAccel = if keys.down == True then -accel else 0
    velocityDelta = upAccel + downAccel
    velocity = add player.velocity (rotate player.direction (0, velocityDelta))

    rotationSpeed = 0.03
    leftDelta = if keys.left == True then -rotationSpeed else 0
    rightDelta = if keys.right == True then rotationSpeed else 0
    directionDelta = leftDelta + rightDelta
    direction = player.direction + directionDelta
  in
    { player
      | position = add position velocity
      , velocity = velocity
      , direction = direction
    }

subscriptions _ =
  let
    left = 37
    right = 39
    up = 38
    down = 40
    space = 32

  in
    Sub.batch
      [ times (\_ -> Tick) -- TODO: Proper delta timing

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
  collage
    width height
    [ rect width height |> filled Color.black
    , drawPlayer model.player
    ]
  |> Element.toHtml

drawPlayer player =
  let
    position = player.position
    direction = player.direction
  in
    group
      -- TODO: Gonna probably need to generalize this wrapping logic
      [ drawShip position direction
      , drawShip (add position (-width, 0)) direction
      , drawShip (add position (width, 0)) direction
      , drawShip (add position (0, -height)) direction
      , drawShip (add position (0, height)) direction
      , drawShip (add position (-width, -height)) direction
      , drawShip (add position (width, -height)) direction
      , drawShip (add position (-width, height)) direction
      , drawShip (add position (width, height)) direction
      ]

drawShip position direction =
  let
    rotate' = rotate direction
    front = add position (rotate' (0, 12))
    left = add position (rotate' (-6, -6))
    right = add position (rotate' (6, -6))
  in
    path [front, left, right, front]
    |> traced { defaultLine | color = Color.white }
