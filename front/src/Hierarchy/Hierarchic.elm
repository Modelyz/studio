module Hierarchy.Hierarchic exposing (Hierarchic, OnlyHierarchic, compare, find, getParentsToRoot, isAscendantOf)

import Dict exposing (Dict)
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias Hierarchic a =
    -- TODO rename Hierarchical
    { a
        | what : Type
        , uuid : Uuid
        , parent : Maybe Uuid
    }


type alias OnlyHierarchic =
    { what : Type
    , uuid : Uuid
    , parent : Maybe Uuid
    }


isAscendantOf : Hierarchic a -> Dict String (Hierarchic a) -> Hierarchic a -> Bool
isAscendantOf child allItems parent =
    -- equality is considered parent TODO reconsider
    if child == parent then
        True

    else
        child.parent
            |> Maybe.andThen (\i -> find allItems i)
            |> Maybe.map (\x -> isAscendantOf x allItems parent)
            |> Maybe.withDefault False


getParent : Dict String (Hierarchic a) -> Hierarchic a -> Maybe (Hierarchic a)
getParent allItems item =
    item.parent
        |> Maybe.andThen (find allItems)


find : Dict String (Hierarchic a) -> Uuid -> Maybe (Hierarchic a)
find es uuid =
    Dict.filter (\_ e -> e.uuid == uuid) es
        |> Dict.values
        |> List.head


getParentsToRoot : Hierarchic a -> Dict String (Hierarchic a) -> List (Hierarchic a) -> List (Hierarchic a)
getParentsToRoot initial allItems currentList =
    getParent allItems initial
        |> Maybe.map (\parent -> getParentsToRoot parent allItems currentList)
        |> Maybe.withDefault currentList


compare : Hierarchic a -> String
compare =
    .uuid >> Uuid.toString
