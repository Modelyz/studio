module Typed.Typed exposing (OnlyTyped, Typed, find, isAscendantOf)

import Dict exposing (Dict)
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Ident.Identifier exposing (Identifier)
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)
import Typed.Type as TType
import Value.Value exposing (Value)



--TODO : turn this into a non parametric type, and turn all types into an extension of Typed (same for Hierarchic)


type alias Typed a =
    { a
      -- TODO try to replace TType.Type whith just Type and rename parent to type_ in the Hierarchic. Eventually merge into Item { what, uuid, type_}
        | what : TType.Type
        , uuid : Uuid
        , type_ : Uuid
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


isAscendantOf : Uuid -> Dict String ( Uuid, Type, Maybe Uuid ) -> Uuid -> Bool
isAscendantOf =
    H.isAscendantOf


find : Dict String (Typed a) -> Uuid -> Maybe (Typed a)
find es uuid =
    Dict.get (Uuid.toString uuid) es
