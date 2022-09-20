module Type exposing (Type(..), compare, decoder, encode, toHierarchic, toString)

import Hierarchy.Type as HType
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Typed.Type as TType


type
    Type
    -- TODO see if it's still useful
    = TType TType.Type
    | HType HType.Type


toString : Type -> String
toString t =
    case t of
        TType tt ->
            TType.toString tt

        HType ht ->
            HType.toString ht


fromString : String -> Maybe Type
fromString s =
    TType.fromString s
        |> Maybe.map TType
        |> (\t ->
                case t of
                    Just tt ->
                        Just tt

                    Nothing ->
                        HType.fromString s
                            |> Maybe.map HType
           )


encode : Type -> Encode.Value
encode =
    toString >> Encode.string


decoder : Decoder Type
decoder =
    Decode.string |> Decode.andThen (fromString >> Maybe.map Decode.succeed >> Maybe.withDefault (Decode.fail "Unknown Type"))


toHierarchic : Type -> Type
toHierarchic t =
    case t of
        TType tt ->
            HType (TType.toHierarchic tt)

        HType ht ->
            HType ht


compare : Type -> String
compare =
    toString
