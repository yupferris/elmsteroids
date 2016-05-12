import List exposing (..)
import Html.App
import Time exposing (..)
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
  , bullets : List Bullet
  , keys : KeyStates
  }

type alias Player =
  { position : Vector
  , velocity : Vector
  , direction : Float
  }

shipFront position direction = add position (rotate direction (0, 12))
shipLeft position direction = add position (rotate direction (-6, -6))
shipRight position direction = add position (rotate direction (6, -6))

type alias Bullet =
  { position : Vector
  , velocity : Vector
  , timeUntilDeath : Float
  }

type alias Vector = (Float, Float)

add x y =
  let
    (xx, xy) = x
    (yx, yy) = y
  in (xx + yx, xy + yy)

mul scalar vector =
  let
    (x, y) = vector
  in (x * scalar, y * scalar)

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
  , spaceTapped : Bool
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
  = None
  | Tick Float -- Time value is always in seconds
  | LeftPressed | LeftReleased
  | RightPressed | RightReleased
  | UpPressed | UpReleased
  | DownPressed | DownReleased
  | SpacePressed

width = 500
height = 500
bounds = (width, height)

update msg model =
  let
    keys = model.keys

    model' =
      case msg of
        None -> model

        Tick timeDelta ->
          { model
            | player = updatePlayer timeDelta keys model.player
            , bullets = updateBullets timeDelta keys model.player model.bullets
            , keys = { keys | spaceTapped = False }
          }

        LeftPressed -> { model | keys = { keys | left = True } }
        LeftReleased -> { model | keys = { keys | left = False } }
        RightPressed -> { model | keys = { keys | right = True } }
        RightReleased -> { model | keys = { keys | right = False } }
        UpPressed -> { model | keys = { keys | up = True } }
        UpReleased -> { model | keys = { keys | up = False } }
        DownPressed -> { model | keys = { keys | down = True } }
        DownReleased -> { model | keys = { keys | down = False } }
        SpacePressed -> { model | keys = { keys | spaceTapped = True } }
  in (model', Cmd.none)

updatePlayer timeDelta keys player =
  let
    position =
      add player.position (mul timeDelta player.velocity)
      |> wrap bounds

    accel = 57.0
    upAccel = if keys.up then accel else 0
    downAccel = if keys.down then -accel else 0
    velocityDelta = upAccel + downAccel
    velocity = add player.velocity (mul timeDelta (rotate player.direction (0, velocityDelta)))

    rotationSpeed = 1.5
    leftDelta = if keys.left then -rotationSpeed else 0
    rightDelta = if keys.right then rotationSpeed else 0
    directionDelta = leftDelta + rightDelta
    direction = player.direction + directionDelta * timeDelta
  in
    { player
      | position = position
      , velocity = velocity
      , direction = direction
    }

updateBullets timeDelta keys player =
  fireBullet keys player >> filterMap (moveBullet timeDelta >> killBullet timeDelta)

fireBullet keys player bullets =
  if keys.spaceTapped then
    { position = shipFront player.position player.direction
    , velocity = player.velocity |> add (rotate player.direction (0, 60))
    , timeUntilDeath = 5.0
    } :: bullets
  else
    bullets

moveBullet timeDelta bullet =
  { bullet | position = add bullet.position (mul timeDelta bullet.velocity) |> wrap bounds }

killBullet timeDelta bullet =
  let
    timeUntilDeath = bullet.timeUntilDeath - timeDelta
  in
    if timeUntilDeath > 0 then Just { bullet | timeUntilDeath = timeUntilDeath }
    else Nothing

subscriptions _ =
  let
    left = 37
    right = 39
    up = 38
    down = 40
    space = 32

  in
    Sub.batch
      [ diffs (inSeconds >> Tick)

      , downs (\key ->
                 -- For some reason case didn't work here
                 -- The compiler thought `left` and `right` were the same patterns (?)
                 if key == left then LeftPressed
                 else if key == right then RightPressed
                 else if key == up then UpPressed
                 else if key == down then DownPressed
                 else if key == space then SpacePressed
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
    , drawBullets model.bullets
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
    front = shipFront position direction
    left = shipLeft position direction
    right = shipRight position direction
  in
    path [front, left, right, front]
    |> traced { defaultLine | color = Color.white }

drawBullets bullets =
  bullets
  |> map (\bullet -> rect 2 2 |> filled Color.white |> move bullet.position)
  |> group
