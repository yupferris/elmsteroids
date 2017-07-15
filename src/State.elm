module State exposing (State, return, (>>=), fmap, ap, (<*>), (<$>))


type alias State s a =
    s -> ( a, s )


return : a -> State s a
return x state =
    ( x, state )


(>>=) : State s a -> (a -> State s b) -> State s b
(>>=) s f =
    \state ->
        let
            ( x, state_ ) =
                s state
        in
            f x state_


fmap : (a -> b) -> State s a -> State s b
fmap f s =
    s
        >>= \state ->
                return (f state)


ap : State s (a -> b) -> State s a -> State s b
ap f x =
    f
        >>= \f_ ->
                x
                    >>= \x_ ->
                            return (f_ x_)


(<*>) : State s (a -> b) -> State s a -> State s b
(<*>) =
    ap


(<$>) : (a -> b) -> State s a -> State s b
(<$>) =
    fmap
