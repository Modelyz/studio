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
    , applyTo : DictSet String Scope
    , unique : Bool
    , mandatory : Bool
    }


within : IdentifierType -> DictSet String Entity -> ( Type, Maybe Uuid ) -> Bool
within identifierType entities tuple =
    identifierType.applyTo
        |> Set.toList
        |> (\scopes ->
                if List.isEmpty scopes then
                    False

                else
                    List.any (\s -> Scope.within s entities tuple) scopes
           )


select : ( Type, Maybe Uuid ) -> DictSet String Entity -> DictSet String IdentifierType -> DictSet String IdentifierType
select ( type_, muuid ) entities its =
    its
        |> Set.filter
            (\it ->
                List.any
                    (\scope ->
                        case scope of
                            AllEntities t ->
                                t == type_

                            AllEntitiesOfType u ->
                                muuid
                                    |> Maybe.andThen (\x -> Entity.findEntity x entities)
                                    |> Maybe.andThen (\e -> Maybe.map (\y -> Entity.isParentOf e entities y) (Entity.findEntity u entities))
                                    |> Maybe.withDefault False
                    )
                    (Set.toList it.applyTo)
            )


compare : IdentifierType -> String
compare it =
    it.name ++ " " ++ (it.applyTo |> Set.toList |> List.map Scope.compare |> String.join " ")


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
