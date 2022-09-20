module IOStatus exposing (IOStatus(..), toText)


type IOStatus
    = IOIdle
    | ESReading
    | ESStoring -- for instance, reading the ES from the MS through WS
    | WSSending
    | IOError String


toText : IOStatus -> String
toText status =
    case status of
        IOIdle ->
            "IOIdle"

        ESReading ->
            "ESReading"

        ESStoring ->
            "ESStoring"

        WSSending ->
            "WSSending"

        IOError err ->
            "IOError: " ++ err
