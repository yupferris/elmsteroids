module State exposing (State, return, andThen, map, map2)

type alias State s a = s -> (a, s)

return : a -> State s a
return x state = (x, state)

andThen : State s a -> (a -> State s b) -> State s b
andThen s f =
  \state ->
    let (x, state') = s state
    in f x state'

map : (a -> b) -> State s a -> State s b
map f s =
  s `andThen` \x ->
    return (f x)

map2 : (a -> b -> value) -> State s a -> State s b -> State s value
map2 f a b = f <$> a <*> b

(<*>) : State s (a -> b) -> State s a -> State s b
(<*>) f x =
  f `andThen` \f' ->
    x `andThen` \x' ->
      return (f' x')

(<$>) : (a -> b) -> State s a -> State s b
(<$>) = map
