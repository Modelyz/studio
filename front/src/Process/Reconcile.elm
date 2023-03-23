module Process.Reconcile exposing (Reconciliation, compare, decoder, encode)

import Expression.Rational as Rational exposing (Rational)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type alias Reconciliation =
    -- a Reconciliation is link between a Process (ie an activity) and a partial Event
    -- TODO use newtype style Uuids (phantom type): type Event = Event Uuid
    { qty : Rational
    , event : Uuid
    , process : Uuid
    }


encode : Reconciliation -> Encode.Value
encode r =
    Encode.object
        [ ( "rational", Rational.encode r.qty )
        , ( "event", Uuid.encode r.event )
        , ( "process", Uuid.encode r.process )
        ]


decoder : Decoder Reconciliation
decoder =
    Decode.map3 Reconciliation
        (Decode.field "rational" Rational.decoder)
        (Decode.field "event" Uuid.decoder)
        (Decode.field "process" Uuid.decoder)


compare : Reconciliation -> String
compare r =
    Uuid.toString r.event ++ "-" ++ Uuid.toString r.process
