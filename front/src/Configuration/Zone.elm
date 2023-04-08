module Configuration.Zone exposing (Zone(..), all, compare, decoder, encode, toDesc)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Zone
    = SmallcardZone
    | MenuZone


all : List Zone
all =
    [ SmallcardZone, MenuZone ]


toString : Zone -> String
toString zone =
    case zone of
        SmallcardZone ->
            "SmallcardZone"

        MenuZone ->
            "MenuZone"


toDesc : Zone -> String
toDesc zone =
    case zone of
        SmallcardZone ->
            "Smallcard Title"

        MenuZone ->
            "Menu Title"


compare : Zone -> String
compare =
    toString


encode : Zone -> Encode.Value
encode zone =
    Encode.string (toString zone)


decoder : Decoder Zone
decoder =
    Decode.string
        |> Decode.andThen
            (\zone ->
                case zone of
                    "SmallcardZone" ->
                        Decode.succeed SmallcardZone

                    "MenuZone" ->
                        Decode.succeed MenuZone

                    _ ->
                        Decode.fail <| "Unknown Zone: " ++ zone
            )
