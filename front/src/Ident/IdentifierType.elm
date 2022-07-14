module Ident.IdentifierType exposing (..)

import DateTime exposing (..)
import DictSet as Set exposing (DictSet)
import Ident.Fragment as Fragment exposing (Fragment)
import Ident.Scope as Scope exposing (Scope)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid
import REA.Entity as EN exposing (Entity(..), toUuid)
import REA.EntityType as ENT exposing (toName)
import Time exposing (Month(..), Posix, Weekday(..), millisToPosix, posixToMillis)


type alias IdentifierType =
    -- This is the configuration of an identifier
    { name : String
    , fragments : List Fragment
    , applyTo : DictSet String Scope
    , unique : Bool
    , mandatory : Bool
    }


compare : IdentifierType -> String
compare =
    .name


encode : IdentifierType -> Encode.Value
encode e =
    Encode.object
        [ ( "name", Encode.string e.name )
        , ( "fragments", Encode.list Fragment.encode e.fragments )
        , ( "applyTo", Encode.list Scope.encode <| Set.toList e.applyTo )
        , ( "unique", Encode.bool e.unique )
        , ( "mandatory", Encode.bool e.mandatory )
        ]


decoder : Decoder IdentifierType
decoder =
    Decode.map5 IdentifierType
        (Decode.field "name" Decode.string)
        (Decode.field "fragments" (Decode.list Fragment.decoder))
        (Decode.field "applyTo" (Decode.list Scope.decoder |> Decode.andThen (Set.fromList Scope.compare >> Decode.succeed)))
        (Decode.field "unique" Decode.bool)
        (Decode.field "mandatory" Decode.bool)


update : Int -> Fragment -> IdentifierType -> IdentifierType
update index fragment identifierType =
    let
        fragments =
            identifierType.fragments
                |> List.indexedMap Tuple.pair
                |> List.map
                    (\( i, f ) ->
                        if i == index then
                            fragment

                        else
                            f
                    )
    in
    { identifierType | fragments = fragments }
