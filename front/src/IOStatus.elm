module IOStatus exposing (IOStatus(..), toEmoji, toText)


type IOStatus
    = IOIdle String
    | ESReading
    | ESStoring -- for instance, reading the ES from the MS through WS
    | WSSending
    | IOError String


toText : IOStatus -> String
toText status =
    case status of
        IOIdle str ->
            "IOIdle (" ++ str ++ ")"

        ESReading ->
            "ESReading"

        ESStoring ->
            "ESStoring"

        WSSending ->
            "WSSending"

        IOError err ->
            "IOError: " ++ err


toEmoji : IOStatus -> String
toEmoji status =
    case status of
        IOIdle _ ->
            "🟢"

        IOError _ ->
            "🔴"

        ESReading ->
            "🔵"

        ESStoring ->
            "🔵"

        WSSending ->
            "🔵"
