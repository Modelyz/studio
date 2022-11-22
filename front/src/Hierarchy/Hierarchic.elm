module Hierarchy.Hierarchic exposing (isAscendantOf)

import Dict exposing (Dict)
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


isAscendantOf : Uuid -> Dict String ( Uuid, Type, Maybe Uuid ) -> Uuid -> Bool
isAscendantOf childUuid types parentUuid =
    if childUuid == parentUuid then
        True

    else
        Dict.get (Uuid.toString childUuid) types
            |> Maybe.andThen (\( _, _, mpuuid ) -> Maybe.map (\p -> isAscendantOf p types parentUuid) mpuuid)
            |> Maybe.withDefault False
