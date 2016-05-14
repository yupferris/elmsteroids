module Bounds exposing (width, height, left, right, top, bottom, bounds)

import Vector exposing (..)

width : Float
width = 854

height : Float
height = 480

left : Float
left = -width / 2

right : Float
right = width / 2

top : Float
top = height / 2

bottom : Float
bottom = -height / 2

bounds : Vector
bounds = (width, height)
