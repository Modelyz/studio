module Hierarchy.Hierarchic exposing (Hierarchic, find, isAscendantOf)

import Dict exposing (Dict)
import Hierarchy.Type as HType
import Ident.Identifier exposing (Identifier)
import Prng.Uuid exposing (Uuid)
import Type exposing (Type)
import Value.Value exposing (Value)


type alias Hierarchic a =
    -- TODO rename Hierarchical
    { a
        | what : HType.Type
        , uuid : Uuid
        , parent : Maybe Uuid
        , identifiers : Dict String Identifier
        , values : Dict String Value
        , display : Dict String String
    }


isAscendantOf : Hierarchic a -> Dict String (Hierarchic a) -> Hierarchic a -> Bool
isAscendantOf child allH parent =
    if child == parent then
        True

    else
        child.parent
            |> Maybe.andThen (\i -> find allH i)
            |> Maybe.map (\x -> isAscendantOf x allH parent)
            |> Maybe.withDefault False


find : Dict String (Hierarchic a) -> Uuid -> Maybe (Hierarchic a)
find hs uuid =
    Dict.filter (\_ e -> e.uuid == uuid) hs
        |> Dict.values
        |> List.head
