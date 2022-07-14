module Ident.Identifier exposing (..)

import DateTime exposing (..)
import DictSet as Set exposing (DictSet)
import Ident.Fragment as Fragment exposing (Fragment)
import Ident.Identifiable as Identifiable exposing (Identifiable)
import Ident.IdentifierType exposing (IdentifierType)
import Ident.Scope exposing (Scope)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid
import REA.Entity as EN exposing (Entity(..), toUuid)
import REA.EntityType as ENT exposing (toName)
import Time exposing (Month(..), Posix, Weekday(..), millisToPosix, posixToMillis)


type alias Identifier =
    -- This is the value of an idenfier (maybe before the entity even exists)
    { name : String
    , fragments : List Fragment
    }


select : String -> DictSet String Identifier -> Maybe Identifier
select name =
    Set.filter (\i -> i.name == name) >> Set.toList >> List.head


compare : Identifier -> String
compare =
    .name


encode : Identifier -> Encode.Value
encode e =
    Encode.object
        [ ( "name", Encode.string e.name )
        , ( "fragments", Encode.list Fragment.encode e.fragments )
        ]


decoder : Decoder Identifier
decoder =
    Decode.map2 Identifier
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


fromIdentifierType : IdentifierType -> Identifier
fromIdentifierType it =
    { name = it.name, fragments = it.fragments }
