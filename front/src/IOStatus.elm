module IOStatus exposing (IOStatus(..), toText)


type IOStatus
    = IOIdle
    | ESReading
    | ESStoring
    | WSReceiving -- for instance, reading the ES from the MS through WS
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

        WSReceiving ->
            "WSReceiving"

        WSSending ->
            "WSSending"

        IOError err ->
            "IOError: " ++ err
