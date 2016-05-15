module State exposing (State, return, (>>=), fmap)

type alias State s a = s -> (a, s)

return : a -> State s a
return x = \state -> (x, state)

(>>=) : State s a -> (a -> State s b) -> State s b
(>>=) sp f =
  \state ->
    let (x, state') = sp state
    in f x state'

fmap : (a -> b) -> State s a -> State s b
fmap f sp =
  sp >>= \state ->
    return (f state)
