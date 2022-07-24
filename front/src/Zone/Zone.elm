module Zone.Zone exposing (Zone(..), all, compare, decoder, encode, toDesc, toString)

import Entity.Type as Type exposing (Type(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import View.Lang as Lang exposing (Lang)


type Zone
    = SmallcardItemTitle
    | SmallcardItemDescription


all : List Zone
all =
    [ SmallcardItemTitle, SmallcardItemDescription ]


toString : Zone -> String
toString zone =
    case zone of
        SmallcardItemTitle ->
            "SmallcardItemTitle"

        SmallcardItemDescription ->
            "SmallcardItemDescription"


toDesc : Zone -> String
toDesc zone =
    case zone of
        SmallcardItemTitle ->
            "Title of the Smallcard Items"

        SmallcardItemDescription ->
            "Description of the Smallcard Items"


compare : Zone -> String
compare =
    toString


display : Zone -> Lang -> Type -> String
display _ _ _ =
    ""


encode : Zone -> Encode.Value
encode zone =
    Encode.string (toString zone)


decoder : Decoder Zone
decoder =
    Decode.string
        |> Decode.andThen
            (\zone ->
                case zone of
                    "SmallcardItemTitle" ->
                        Decode.succeed SmallcardItemTitle

                    "SmallcardItemDescription" ->
                        Decode.succeed SmallcardItemDescription

                    _ ->
                        Decode.fail <| "Unknown Zone: " ++ zone
            )
