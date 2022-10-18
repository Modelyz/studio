module Value.Commitment exposing (and, give, one, scale, zero)

import Commitment.Commitment as C exposing (Commitment)
import Dict exposing (Dict)
import Value.Rational as R exposing (Rational)



-- TODO remove?


type alias Contract =
    Dict String Commitment


zero : Contract
zero =
    Dict.empty


one : Commitment -> Contract
one c =
    Dict.singleton (C.compare c) c


give : Contract -> Contract
give =
    -- FIXME the modified commitments should not have the same uuid!!!
    -- maybe remove the uuid from the commitment?
    -- maybe we're not dealing with commitments?
    Dict.map
        (\_ cm ->
            { cm | provider = cm.receiver, receiver = cm.provider }
        )


scale : Rational -> Contract -> Contract
scale r =
    Dict.map
        (\_ cm ->
            { cm | qty = R.mul r cm.qty }
        )


and : Contract -> Contract -> Contract
and =
    Dict.union
