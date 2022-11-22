module Monad.Reader exposing (Reader(..), andThen, ask, join, map, reader, return, run, unwrap)


type Reader s a
    = Reader (s -> a)


reader : (s -> a) -> Reader s a
reader =
    Reader


run : s -> Reader s a -> a
run state (Reader r) =
    r state


ask : Reader s s
ask =
    Reader identity


return : a -> Reader s a
return x =
    Reader (\_ -> x)


unwrap : Reader s a -> (s -> a)
unwrap (Reader r) =
    r


andThen : (a -> Reader s b) -> Reader s a -> Reader s b
andThen f =
    map f >> join


join : Reader s (Reader s a) -> Reader s a
join r =
    Reader (\state -> unwrap r state |> (\rb -> unwrap rb state))


map : (a -> b) -> Reader s a -> Reader s b
map f (Reader r) =
    Reader (r >> f)
