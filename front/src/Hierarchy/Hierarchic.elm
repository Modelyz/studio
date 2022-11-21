module Hierarchy.Hierarchic exposing (Hierarchic, find, isAscendantOf)

import Dict exposing (Dict)
import Hierarchy.Type as HType
import Ident.Identifier exposing (Identifier)
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)
import Value.Value exposing (Value)


type alias Hierarchic a =
    -- TODO rename Hierarchical
    { a
        | what : HType.Type
        , uuid : Uuid
        , parent : Maybe Uuid
    }


isAscendantOf : Uuid -> Dict String ( Uuid, Type, Maybe Uuid ) -> Uuid -> Bool
isAscendantOf childUuid types parentUuid =
    if childUuid == parentUuid then
        True

    else
        Dict.get (Uuid.toString childUuid) types
            |> Maybe.andThen (\( _, _, mpuuid ) -> Maybe.map (\p -> isAscendantOf p types parentUuid) mpuuid)
            |> Maybe.withDefault False


find : Dict String (Hierarchic a) -> Uuid -> Maybe (Hierarchic a)
find hs uuid =
    Dict.get (Uuid.toString uuid) hs
