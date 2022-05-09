module REA.Identifier exposing (..)

import REA.Entity exposing (Entity)
import REA.Identifier.Portion exposing (Portion)



-- Where do we get the date from
-- The IDntifier is the definition of an identifier


type alias Identifier =
    { name : String
    , entity : Entity
    , unique : Bool
    , mandatory : Bool
    , format : List Portion
    }



-- The identifier can be applied on:


compare : Identifier -> String
compare =
    .name
