module Ident.IdentifierType exposing (IdentifierType, compare, decoder, encode, initIdentifiers)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Ident.Fragment as Fragment exposing (Fragment)
import Ident.Identifier as Identifier exposing (Identifier)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Type exposing (Type)
import Typed.Type as TType
import Typed.Typed exposing (Typed)


type alias IdentifierType =
    -- This is the definition of an identifier
    { name : String
    , fragments : List Fragment
    , scope : Scope
    , unique : Bool
    , mandatory : Bool
    }


initIdentifiers : Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String IdentifierType -> Type -> Maybe Uuid -> Uuid -> Bool -> Dict String Identifier
initIdentifiers types its t muuid uuid isNew =
    {- build the empty identifiers corresponding to the chosen type, possible user type, and uuid of the added/edited entity
       if uuid is a newly generated one, we only have the Agent type t, the selected parent type and No uuid.
       So we must find all the it whose scope is ascendent of (HasUserType t h.what h.uuid).
    -}
    (its
        |> Dict.filter
            (\_ it ->
                if isNew then
                    containsScope types
                        (Maybe.map (HasUserType t) muuid
                            |> Maybe.withDefault (HasType t)
                        )
                        it.scope

                else
                    containsScope types (IsItem t uuid) it.scope
            )
    )
        |> Dict.values
        |> List.map
            (\it ->
                let
                    i =
                        Identifier t uuid it.name it.fragments
                in
                ( Identifier.compare i, i )
            )
        |> Dict.fromList


compare : IdentifierType -> String
compare it =
    Scope.compare it.scope ++ "/" ++ it.name


encode : IdentifierType -> Encode.Value
encode e =
    Encode.object
        [ ( "name", Encode.string e.name )
        , ( "fragments", Encode.list Fragment.encode e.fragments )
        , ( "scope", Scope.encode e.scope )
        , ( "unique", Encode.bool e.unique )
        , ( "mandatory", Encode.bool e.mandatory )
        ]


decoder : Decoder IdentifierType
decoder =
    Decode.map5 IdentifierType
        (Decode.field "name" Decode.string)
        (Decode.field "fragments" (Decode.list Fragment.decoder))
        (Decode.field "scope" Scope.decoder)
        (Decode.field "unique" Decode.bool)
        (Decode.field "mandatory" Decode.bool)
