module View.Type exposing (Type(..), decoder, encode, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Type
    = Smallcard
    | New


toString : Type -> String
toString t =
    case t of
        Smallcard ->
            "Smallcard"

        New ->
            "New"


encode : Type -> Encode.Value
encode t =
    Encode.string <| toString t


decoder : Decoder Type
decoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Smallcard" ->
                        Decode.succeed Smallcard

                    "New" ->
                        Decode.succeed New

                    _ ->
                        Decode.fail <| "Unknown View Type: " ++ s
            )
