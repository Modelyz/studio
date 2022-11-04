module Typed.Typed exposing (OnlyTyped, Typed, find, isAscendantOf)

import Dict exposing (Dict)
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Ident.Identifier exposing (Identifier)
import Prng.Uuid exposing (Uuid)
import Type exposing (Type)
import Typed.Type as TType
import Value.Value exposing (Value)



--TODO : turn this into a non parametric type, and turn all types into an extension of Typed (same for Hierarchic)


type alias Typed a =
    { a
        | what : TType.Type
        , uuid : Uuid
        , type_ : Uuid
        , identifiers : Dict String Identifier
        , values : Dict String Value
        , display : Dict String String
    }


type alias OnlyTyped =
    -- this seems necessary because all the Typed types have not the same fields (see the end of State.elm)
    { what : TType.Type
    , uuid : Uuid
    , type_ : Uuid
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , display : Dict String String
    }


isAscendantOf : Typed a -> Dict String (Hierarchic b) -> Hierarchic b -> Bool
isAscendantOf childT allH parentH =
    Maybe.map (\t -> H.isAscendantOf t allH parentH) (H.find allH childT.type_)
        |> Maybe.withDefault False


find : Dict String (Typed a) -> Uuid -> Maybe (Typed a)
find es uuid =
    Dict.filter (\_ e -> e.uuid == uuid) es
        |> Dict.values
        |> List.head
