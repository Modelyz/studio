module Ident.IdentifierType exposing (..)

import DictSet as Set exposing (DictSet)
import Entity.Entity as Entity exposing (Entity)
import Entity.Type as Type exposing (Type)
import Ident.Fragment as Fragment exposing (Fragment)
import Ident.Scope as Scope exposing (Scope(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type alias IdentifierType =
    -- This is the configuration of an identifier
    { name : String
    , fragments : List Fragment
    , applyTo : Scope
    , unique : Bool
    , mandatory : Bool
    }



--within : IdentifierType -> DictSet String Entity -> ( Type, Maybe Uuid ) -> Bool
--within identifierType entities tuple =
--    -- TODO remove?
--    Scope.within identifierType.applyTo entities tuple


select : ( Type, Maybe Uuid ) -> DictSet String Entity -> DictSet String IdentifierType -> DictSet String IdentifierType
select ( type_, muuid ) entities its =
    its
        |> Set.filter
            (\it ->
                case it.applyTo of
                    AllEntities t ->
                        t == type_

                    AllEntitiesOfType t u ->
                        (t == type_)
                            && (muuid
                                    |> Maybe.andThen (\x -> Entity.findEntity x entities)
                                    |> Maybe.andThen (\e -> Maybe.map (\y -> Entity.isParentOf e entities y) (Entity.findEntity u entities))
                                    |> Maybe.withDefault False
                               )
            )


compare : IdentifierType -> String
compare it =
    it.name ++ " " ++ Scope.compare it.applyTo


encode : IdentifierType -> Encode.Value
encode e =
    Encode.object
        [ ( "name", Encode.string e.name )
        , ( "fragments", Encode.list Fragment.encode e.fragments )
        , ( "applyTo", Scope.encode e.applyTo )
        , ( "unique", Encode.bool e.unique )
        , ( "mandatory", Encode.bool e.mandatory )
        ]


decoder : Decoder IdentifierType
decoder =
    Decode.map5 IdentifierType
        (Decode.field "name" Decode.string)
        (Decode.field "fragments" (Decode.list Fragment.decoder))
        (Decode.field "applyTo" Scope.decoder)
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
