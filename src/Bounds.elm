module Bounds exposing (width, height, left, right, top, bottom, safeZoneSize)


width : Float
width =
    854


height : Float
height =
    480


left : Float
left =
    -width / 2


right : Float
right =
    width / 2


top : Float
top =
    height / 2


bottom : Float
bottom =
    -height / 2


safeZoneSize : Float
safeZoneSize =
    100
