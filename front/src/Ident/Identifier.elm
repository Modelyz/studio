module Ident.Identifier exposing (..)

import DictSet as Set exposing (DictSet)
import Entity.Entity as Entity exposing (Entity, toUuid)
import Ident.Fragment as Fragment exposing (Fragment)
import Ident.IdentifierType exposing (IdentifierType)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type alias Identifier =
    -- This is the value of an identifier
    { entity : Uuid
    , name : String
    , fragments : List Fragment
    }


select : String -> DictSet String Identifier -> Maybe Identifier
select name =
    Set.filter (\i -> i.name == name) >> Set.toList >> List.head


fromEntity : Entity -> DictSet String Identifier -> List Identifier
fromEntity entity =
    -- return the identifiers corresponding to a certain entity
    Set.filter (\i -> toUuid entity == i.entity) >> Set.toList


restrict =
    -- TODO remove
    fromEntity


compare : Identifier -> String
compare i =
    Uuid.toString i.entity ++ " " ++ i.name


encode : Identifier -> Encode.Value
encode e =
    Encode.object
        [ ( "entity", Uuid.encode e.entity )
        , ( "name", Encode.string e.name )
        , ( "fragments", Encode.list Fragment.encode e.fragments )
        ]


decoder : Decoder Identifier
decoder =
    Decode.map3 Identifier
        (Decode.field "entity" Uuid.decoder)
        (Decode.field "name" Decode.string)
        (Decode.field "fragments" (Decode.list Fragment.decoder))


toValue : Identifier -> String
toValue i =
    i.fragments |> List.map Fragment.toValue |> String.concat


update : Int -> Fragment -> Identifier -> Identifier
update index fragment identifier =
    let
        fragments =
            identifier.fragments
                |> List.indexedMap Tuple.pair
                |> List.map
                    (\( i, f ) ->
                        if i == index then
                            fragment

                        else
                            f
                    )
    in
    { identifier | fragments = fragments }


fromIdentifierType : Uuid -> IdentifierType -> Identifier
fromIdentifierType entity it =
    Identifier entity it.name it.fragments


match : String -> Identifier -> Bool
match string identifier =
    -- True if the string is contained in any fragment of the identifier
    Fragment.matchAny string identifier.fragments
