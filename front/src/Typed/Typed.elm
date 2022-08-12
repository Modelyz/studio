module Typed.Typed exposing (OnlyTyped, Typed, compare, find, isAscendantOf)

import Dict exposing (Dict)
import Hierarchy.Hierarchic as Hierarchic exposing (Hierarchic)
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias Typed a =
    { a
        | what : Type
        , uuid : Uuid
        , type_ : Uuid
        , identifiers : Dict String String
    }


type alias OnlyTyped =
    -- this seems necessary because all the Typed types have not the same field (see the end of State.elm)
    { what : Type
    , uuid : Uuid
    , type_ : Uuid
    , identifiers : Dict String String
    }


isAscendantOf : Typed a -> Dict String (Hierarchic b) -> Hierarchic b -> Bool
isAscendantOf typed allItems someType =
    Maybe.map (\t -> Hierarchic.isAscendantOf t allItems someType) (Hierarchic.find allItems typed.type_)
        |> Maybe.withDefault False


find : Dict String (Typed a) -> Uuid -> Maybe (Typed a)
find es uuid =
    Dict.filter (\_ e -> e.uuid == uuid) es
        |> Dict.values
        |> List.head


compare : Typed a -> String
compare =
    .uuid >> Uuid.toString
