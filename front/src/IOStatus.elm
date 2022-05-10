module IOStatus exposing (IOStatus(..), toEmoji, toText)


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


toEmoji : IOStatus -> String
toEmoji status =
    case status of
        IOIdle ->
            "🔵"

        ESReading ->
            "📤"

        ESStoring ->
            "📥"

        WSReceiving ->
            "🔻"

        WSSending ->
            "🔺"

        IOError err ->
            "🔴 (" ++ err ++ ")"
