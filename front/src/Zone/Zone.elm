module Zone.Zone exposing (Zone(..), all, compare, decoder, encode, toDesc, toString)

import Dict exposing (Dict)
import Entity.Entity as Entity exposing (Entity)
import Entity.Type as Type exposing (Type(..))
import Ident.Identifier as Identifier
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import View.Lang as Lang exposing (Lang)


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
            "Title of the Smallcard Items"

        SmallcardDescription ->
            "Description of the Smallcard Items"


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
