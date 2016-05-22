module SegmentParticles exposing (SegmentParticle, segmentParticles, tick, draw)

import List exposing (map, filterMap)
import Collage exposing (Form, group, path, traced, defaultLine, move, alpha)
import Color exposing (..)
import Random exposing (Seed, float, step)
import State exposing (State, finally, andThen, map2)
import Vector exposing (..)
import Segment exposing (Segment, center)

type alias SegmentParticle =
  { position : Vector
  , velocity : Vector
  , rotation : Float
  , rotationVelocity : Float
  , segment : Segment
  , timeUntilDeath : Float
  }

segmentParticles : Vector -> List Segment -> State Seed (List SegmentParticle)
segmentParticles initialVelocity segments =
  case segments of
    [] -> finally []
    x::xs ->
      map2
        (::)
        (segmentParticle initialVelocity x)
        (segmentParticles initialVelocity xs)

segmentParticle : Vector -> Segment -> State Seed SegmentParticle
segmentParticle initialVelocity segment =
  let angle = float 0 (pi * 2) |> step
  in
    angle `andThen` \velDirection ->
      step (float 0 40) `andThen` \velMagnitude ->
        step (float -1 1) `andThen` \rotationVelocity ->
          step (float 1 3) `andThen` \timeUntilDeath ->
            let
              position = center segment
              segment' =
                { a = sub segment.a position
                , b = sub segment.b position
                }
            in
              finally
                { position = position
                , velocity =
                  rotate velDirection (0, velMagnitude)
                    |> add initialVelocity
                , rotation = 0
                , rotationVelocity = rotationVelocity
                , segment = segment'
                , timeUntilDeath = timeUntilDeath
                }

tick : Float -> List SegmentParticle -> List SegmentParticle
tick timeDelta =
  filterMap (moveParticle timeDelta >> rotateParticle timeDelta >> killParticle timeDelta)

moveParticle : Float -> SegmentParticle -> SegmentParticle
moveParticle timeDelta particle =
  { particle | position =
      add particle.position (mulS timeDelta particle.velocity) |> wrap
  }

rotateParticle : Float -> SegmentParticle -> SegmentParticle
rotateParticle timeDelta particle =
  { particle | rotation =
      particle.rotation + particle.rotationVelocity * timeDelta
  }

killParticle : Float -> SegmentParticle -> Maybe SegmentParticle
killParticle timeDelta particle =
  let timeUntilDeath = particle.timeUntilDeath - timeDelta
  in
    if timeUntilDeath > 0 then
      Just { particle | timeUntilDeath = timeUntilDeath }
    else Nothing

draw : List SegmentParticle -> Form
draw = map drawParticle >> group

drawParticle : SegmentParticle -> Form
drawParticle particle =
  particle.segment
    |> Segment.wrap
    |> map (drawSegment particle.rotation)
    |> group
    |> move particle.position
    |> alpha (min particle.timeUntilDeath 1)

drawSegment : Float -> Segment -> Form
drawSegment rotation segment =
  path
    [ rotate rotation segment.a
    , rotate rotation segment.b
    ]
    |> traced { defaultLine | color = white }
