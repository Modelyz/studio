module REA.Identification exposing (..)

import REA.Entity exposing (Entity)
import REA.Identification.Portion exposing (Portion)



-- Where do we get the date from
-- The IDntifier is the definition of an identification


type alias Identification =
    { name : String
    , entity : Entity
    , unique : Bool
    , mandatory : Bool
    , format : List Portion
    }



-- The identification can be applied on:


compare : Identification -> String
compare =
    .name
