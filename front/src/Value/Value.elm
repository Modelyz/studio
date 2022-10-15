module Value.Value exposing (Value, compare, fromUuid, toValue)

import Dict exposing (Dict)
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)
import Value.Expression as Expression exposing (Expression)
import Value.Observable exposing (Observable)


type alias Value =
    { what : Type
    , for : Uuid
    , name : String
    , expr : Expression Observable
    }


fromUuid : Uuid -> Dict String Value -> Dict String Value
fromUuid uuid =
    Dict.filter (\_ i -> uuid == i.for)


compare : Value -> String
compare v =
    Type.compare v.what ++ "/" ++ Uuid.toString v.for ++ "/" ++ v.name


toValue : Value -> Result String Int
toValue val =
    -- TODO move to Rational
    Expression.eval val.expr
