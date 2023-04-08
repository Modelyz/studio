module Configuration.Zone exposing (Zone(..), all, compare, decoder, encode, toDesc)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Zone
    = SmallcardTitle
    | SmallcardDescription


all : List Zone
all =
    [ SmallcardTitle, SmallcardDescription ]


toString : Zone -> String
toString zone =
    case zone of
        SmallcardTitle ->
            "SmallcardTitle"

        SmallcardDescription ->
            "SmallcardDescription"


toDesc : Zone -> String
toDesc zone =
    case zone of
        SmallcardTitle ->
            "Smallcard Title"

        SmallcardDescription ->
            "Smallcard Description"


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
                    "SmallcardTitle" ->
                        Decode.succeed SmallcardTitle

                    "SmallcardDescription" ->
                        Decode.succeed SmallcardDescription

                    _ ->
                        Decode.fail <| "Unknown Zone: " ++ zone
            )
