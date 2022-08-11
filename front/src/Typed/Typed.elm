module Typed.Typed exposing (Typed, compare, isAscendantOf)

import DictSet as Set exposing (DictSet)
import Hierarchy.Hierarchic as Hierarchic exposing (Hierarchic)
import Item.Item as Item exposing (Item)
import Prng.Uuid as Uuid exposing (Uuid)


type alias Typed a =
    { a | type_ : Uuid }


isAscendantOf : Typed (Item a) -> DictSet String (Hierarchic (Item b)) -> Hierarchic (Item b) -> Bool
isAscendantOf typed allItems someType =
    Maybe.map (\t -> Hierarchic.isAscendantOf t allItems someType) (Item.find allItems typed.type_)
        |> Maybe.withDefault False


compare : Typed (Item a) -> String
compare =
    Item.compare
