module Type exposing (Type(..), compare, decoder, encode, parentOf, toHierarchic, toPluralString, toString, typeOf)

import Dict exposing (Dict)
import Hierarchy.Type as HType
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
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


typeOf : Dict String { a | type_ : Uuid } -> Dict String { b | uuid : Uuid } -> Uuid -> List Uuid
typeOf entities types uuid =
    Dict.get (Uuid.toString uuid) entities
        |> Maybe.map
            (\e ->
                types
                    |> Dict.filter (\_ v -> v.uuid == e.type_)
                    |> Dict.values
                    |> List.map .uuid
            )
        |> Maybe.withDefault []


parentOf : Dict String { a | uuid : Uuid, parent : Maybe Uuid } -> Uuid -> List Uuid
parentOf types uuid =
    Dict.get (Uuid.toString uuid) types
        |> Maybe.map
            (\e ->
                types
                    |> Dict.filter (\_ t -> e.parent |> Maybe.map ((==) t.uuid) |> Maybe.withDefault False)
                    |> Dict.values
                    |> List.map .uuid
            )
        |> Maybe.withDefault []
