module Ident.IdentifierType exposing (IdentifierType, compare, decoder, encode, initIdentifiers)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Hierarchy.Hierarchic exposing (Hierarchic)
import Ident.Fragment as Fragment exposing (Fragment)
import Ident.Identifier as Identifier exposing (Identifier)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope(..))
import Type exposing (Type)
import Typed.Typed exposing (Typed)


type alias IdentifierType =
    -- This is the definition of an identifier
    { name : String
    , fragments : List Fragment
    , applyTo : Scope -- TODO rename to scope
    , unique : Bool
    , mandatory : Bool
    }


initIdentifiers : Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String IdentifierType -> Type -> Maybe (Hierarchic h) -> Uuid -> Dict String Identifier
initIdentifiers allT allH its t mh newUuid =
    -- build the empty identifiers corresponding to the chosen type and possible user type
    let
        hscope =
            -- scope corresponding to the hierarchic entity we're dealing with
            Maybe.map (\h -> HasUserType t h.uuid) mh |> Maybe.withDefault (HasType t)
    in
    its
        |> Dict.filter (\_ it -> Scope.containsScope allT allH hscope it.applyTo)
        |> Dict.values
        |> List.map
            (\it ->
                let
                    i =
                        Identifier t newUuid it.name it.fragments
                in
                ( Identifier.compare i, i )
            )
        |> Dict.fromList


compare : IdentifierType -> String
compare it =
    Scope.compare it.applyTo ++ "|" ++ it.name


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
