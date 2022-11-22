module Ident.Identifiable exposing (getIdentifiers)

import Dict exposing (Dict)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType exposing (IdentifierType, initIdentifiers)
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)



-- TODO move to somewhere else (see the same for Value) and remove this module
-- TODO or restore a separated Identifiable type?


getIdentifiers : Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String IdentifierType -> Dict String Identifier -> Type -> Uuid -> Maybe Uuid -> Bool -> Dict String Identifier
getIdentifiers types allIdts allIds t uuid mpuuid isNew =
    -- start with empty identifiers from identifierTypes, then merge with existing identifiers
    initIdentifiers types allIdts t mpuuid uuid isNew
        |> Dict.union (Identifier.fromUuid uuid allIds)
