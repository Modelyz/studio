module Hierarchy.Hierarchic exposing (Hierarchic, compare, getParentsToRoot, isAscendantOf)

import DictSet as Set exposing (DictSet)
import Item.Item as Item exposing (Item, find)
import Prng.Uuid as Uuid exposing (Uuid)


type alias Hierarchic a =
    -- TODO rename Hierarchical
    { a | parent : Maybe Uuid }


isAscendantOf : Hierarchic (Item a) -> DictSet String (Hierarchic (Item a)) -> Hierarchic (Item a) -> Bool
isAscendantOf child allItems parent =
    -- equality is considered parent TODO reconsider
    if child == parent then
        True

    else
        child.parent
            |> Maybe.andThen (\i -> find allItems i)
            |> Maybe.map (\x -> isAscendantOf x allItems parent)
            |> Maybe.withDefault False


getParent : DictSet String (Hierarchic (Item a)) -> Hierarchic (Item a) -> Maybe (Hierarchic (Item a))
getParent allItems item =
    item.parent
        |> Maybe.andThen (Item.find allItems)


getParentsToRoot : Hierarchic (Item a) -> DictSet String (Hierarchic (Item a)) -> List (Hierarchic (Item a)) -> List (Hierarchic (Item a))
getParentsToRoot initial allItems currentList =
    getParent allItems initial
        |> Maybe.map (\parent -> getParentsToRoot parent allItems currentList)
        |> Maybe.withDefault currentList


compare : Hierarchic (Item a) -> String
compare =
    Item.compare
