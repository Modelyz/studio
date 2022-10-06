module Value.Value exposing (Value, compare)

import Dict exposing (Dict)
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)
import Value.Expression exposing (Expression)
import Value.Observable exposing (Observable)


type alias Value =
    { what : Type
    , for : Uuid
    , name : String
    , expr : Expression Observable
    }


compare : Value -> String
compare v =
    Type.compare v.what ++ "/" ++ Uuid.toString v.for ++ "/" ++ v.name
