module View.Zone exposing (Zone, compare, decoder, encode)

import Entity.Type as Type exposing (Type)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import View.Lang as Lang exposing (Lang)


type Zone
    = SmallcardItemTitle
    | SmallcardItemDescription
    | SmallcardPageTitle


toString : Zone -> String
toString zone =
    case zone of
        SmallcardItemTitle ->
            "SmallcardItemTitle"

        SmallcardItemDescription ->
            "SmallcardItemDescription"

        SmallcardPageTitle ->
            "SmallcardPageTitle"


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

                    "SmallcardPageTitle" ->
                        Decode.succeed SmallcardPageTitle

                    _ ->
                        Decode.fail <| "Unknown Zone: " ++ zone
            )
