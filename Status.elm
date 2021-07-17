module Status exposing (Status(..))


type Status a
    = Loading
    | Failed String
    | Loaded a
