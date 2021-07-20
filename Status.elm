module Status exposing (Status(..))


type Status
    = Loading
    | Failed String
    | Loaded
