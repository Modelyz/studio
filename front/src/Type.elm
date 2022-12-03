module Type exposing (Type(..), compare, decoder, encode, isChildOf, isParentOf, toHierarchic, toPluralString, toString, typeOf)

import Dict exposing (Dict)
import Hierarchy.Type as HType
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Typed.Type as TType
import Util exposing (third)


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


toPluralString : Type -> String
toPluralString t =
    case t of
        TType tt ->
            TType.toPluralString tt

        HType ht ->
            HType.toPluralString ht


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


typeOf : Dict String ( Uuid, Type, Maybe Uuid ) -> Uuid -> Maybe Uuid
typeOf types uuid =
    Dict.get (Uuid.toString uuid) types |> Maybe.andThen third


isChildOf : Dict String ( Uuid, Type, Maybe Uuid ) -> Uuid -> Uuid -> Bool
isChildOf types parent uuid =
    if uuid == parent then
        True

    else
        Dict.get (Uuid.toString uuid) types
            |> Maybe.andThen third
            |> Maybe.map
                (\puuid ->
                    if parent == puuid then
                        True

                    else
                        isChildOf types parent puuid
                )
            |> Maybe.withDefault False


isParentOf : Dict String ( Uuid, Type, Maybe Uuid ) -> Uuid -> Uuid -> Bool
isParentOf types parent uuid =
    isChildOf types uuid parent
