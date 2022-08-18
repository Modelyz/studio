module Ident.IdentifierType exposing (..)

import Dict exposing (Dict)
import Hierarchy.Hierarchic as Hierarchic exposing (Hierarchic)
import Ident.Fragment as Fragment exposing (Fragment)
import Ident.Identifier as Identifier exposing (Identifier)
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


initIdentifiers : Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String IdentifierType -> Type -> Maybe (Hierarchic b) -> Uuid -> Dict String Identifier
initIdentifiers allT allH its t mh newUuid =
    -- build the empty identifiers corresponding to the chosen type and possible user type
    let
        scope =
            Maybe.map (\h -> HasUserType t h.uuid) mh |> Maybe.withDefault (HasType t)
    in
    (its
        |> Dict.filter (\_ it -> Scope.containsScope allT allH scope it.applyTo)
    )
        |> Dict.values
        |> List.map (\it -> ( Uuid.toString newUuid ++ "/" ++ it.name, Identifier newUuid it.name it.fragments ))
        |> Dict.fromList


compare : IdentifierType -> String
compare it =
    Scope.compare it.applyTo ++ "/" ++ it.name


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
