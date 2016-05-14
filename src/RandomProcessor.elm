module RandomProcessor exposing (RandomProcessor, return, (>>=))

import Random exposing (Seed)

type alias RandomProcessor a = Seed -> (a, Seed)

return : a -> RandomProcessor a
return x = \seed -> (x, seed)

(>>=) : RandomProcessor a -> (a -> RandomProcessor b) -> RandomProcessor b
(>>=) rp f =
  \seed ->
    let (x, seed') = rp seed
    in f x seed'
