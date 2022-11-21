module Ident.Identifiable exposing (getIdentifiers, withIdentifiers)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType exposing (IdentifierType, initIdentifiers)
import Item.Item exposing (Item)
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope(..))
import State exposing (State)
import Type exposing (Type)
import Typed.Typed exposing (Typed)



-- TODO move to somewhere else (see the same for Value) and remove this module
-- TODO or restore a separated Identifiable type?


getIdentifiers : Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String IdentifierType -> Dict String Identifier -> Type -> Uuid -> Dict String Identifier
getIdentifiers types allIdts allIds t uuid =
    -- start with empty identifiers from identifierTypes, then merge with existing identifiers
    initIdentifiers types allIdts t (Maybe.andThen (\( _, _, x ) -> x) (Dict.get (Uuid.toString uuid) types)) uuid False
        |> Dict.union (Identifier.fromUuid uuid allIds)


withIdentifiers : State -> Dict String { a | uuid : Uuid, identifiers : Dict String Identifier } -> Dict String { a | uuid : Uuid, identifiers : Dict String Identifier }
withIdentifiers state =
    Dict.map (\_ x -> { x | identifiers = state.identifiers |> Dict.filter (\_ id -> x.uuid == id.identifiable) })
