module State exposing (State, return, (>>=))

type alias State s a = s -> (a, s)

return : a -> State s a
return x = \state -> (x, state)

(>>=) : State s a -> (a -> State s b) -> State s b
(>>=) sp f =
  \state ->
    let (x, state') = sp state
    in f x state'
