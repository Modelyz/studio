module Ident.IdentifierType exposing (..)

import DictSet as Set exposing (DictSet)
import Hierarchy.Hierarchic as Hierarchic exposing (Hierarchic)
import Hierarchy.Type as HType
import Ident.Fragment as Fragment exposing (Fragment)
import Ident.Identifiable as Identifiable exposing (Identifiable)
import Ident.Identifier as Identifier exposing (Identifier)
import Item.Item as Item exposing (Item)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope(..))
import Type exposing (Type)
import Typed.Typed as TType exposing (Typed)


type alias IdentifierType =
    -- This is the definition of an identifier
    { name : String
    , fragments : List Fragment
    , applyTo : Scope
    , unique : Bool
    , mandatory : Bool
    }


toIdentifier : Uuid -> IdentifierType -> Identifier
toIdentifier uuid it =
    Identifier uuid it.name it.fragments


select : Scope -> DictSet String (Typed (Item a)) -> DictSet String (Hierarchic (Item b)) -> DictSet String IdentifierType -> DictSet String IdentifierType
select scope allTyped allHierarchic its =
    -- keep the identifiertypes corresponding to the Type or user type of the identifiable
    Set.filter (\it -> Scope.containsScope allTyped allHierarchic scope it.applyTo) its


initIdentifiers : DictSet String (Typed (Item a)) -> DictSet String (Hierarchic (Item b)) -> DictSet String IdentifierType -> Type -> Maybe (Hierarchic (Item b)) -> Uuid -> DictSet String Identifier
initIdentifiers allTyped allHierarchic identifierTypes t mh newUuid =
    -- build the empty identifiers corresponding to the chosen type and possible user type
    identifierTypes
        |> select (Maybe.map (\h -> And (IsType t) (HasUserType h.uuid)) mh |> Maybe.withDefault (IsType t)) allTyped allHierarchic
        |> Set.map Identifier.compare (toIdentifier newUuid)


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
