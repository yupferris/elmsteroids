module Vector exposing (Vector, length, normalize, add, sub, mul, mulS, div, divS, dot, cross, rotate, wrap)

type alias Vector = (Float, Float)

length : Vector -> Float
length vector =
  let (x, y) = vector
  in sqrt (x ^ 2 + y ^ 2)

normalize : Vector -> Vector
normalize vector =
  divS (length vector) vector

add : Vector -> Vector -> Vector
add x y =
  let
    (xx, xy) = x
    (yx, yy) = y
  in (xx + yx, xy + yy)

sub : Vector -> Vector -> Vector
sub x y =
  let
    (xx, xy) = x
    (yx, yy) = y
  in (xx - yx, xy - yy)

mul : Vector -> Vector -> Vector
mul x y =
  let
    (xx, xy) = x
    (yx, yy) = y
  in (xx * yx, xy * yy)

mulS : Float -> Vector -> Vector
mulS scalar vector =
  let (x, y) = vector
  in (x * scalar, y * scalar)

div : Vector -> Vector -> Vector
div x y =
  let
    (xx, xy) = x
    (yx, yy) = y
  in (xx / yx, xy / yy)

divS : Float -> Vector -> Vector
divS scalar vector =
  let (x, y) = vector
  in (x / scalar, y / scalar)

dot : Vector -> Vector -> Float
dot x y =
  let
    (xx, xy) = x
    (yx, yy) = y
  in xx * yx + xy * yy

cross : Vector -> Vector -> Float
cross x y =
  let
    (xx, xy) = x
    (yx, yy) = y
  in xx * yy - xy * yx

rotate : Float -> Vector -> Vector
rotate angle vector =
  let
    (x, y) = vector
    c = cos angle
    s = sin angle
  in (x * c + y * s, y * c - x * s)

wrap : Vector -> Vector -> Vector
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
