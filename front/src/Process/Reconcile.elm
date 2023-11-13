module Process.Reconcile exposing (Reconciliation, byProcess, compare, decoder, encode, filterByEvent, fromAllocations, toAllocations)

import Dict exposing (Dict)
import Element exposing (..)
import Expression.Rational as Rational exposing (Rational)
import Expression.RationalInput exposing (RationalInput)
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


byProcess : Uuid -> Dict String Reconciliation -> Dict String Reconciliation
byProcess uuid =
    Dict.filter (\_ r -> r.process == uuid)


filterByEvent : Uuid -> Dict String Reconciliation -> Dict String Reconciliation
filterByEvent uuid =
    Dict.filter (\_ r -> r.event == uuid)


encode : Reconciliation -> Encode.Value
encode r =
    Encode.object
        [ ( "qty", Rational.encode r.qty )
        , ( "event", Uuid.encode r.event )
        , ( "process", Uuid.encode r.process )
        ]


decoder : Decoder Reconciliation
decoder =
    Decode.map3 Reconciliation
        (Decode.field "qty" Rational.decoder)
        (Decode.field "event" Uuid.decoder)
        (Decode.field "process" Uuid.decoder)


compare : Reconciliation -> String
compare r =
    Uuid.toString r.event ++ "-" ++ Uuid.toString r.process


toAllocations : Dict String Reconciliation -> List ( Uuid, RationalInput )
toAllocations reconciliations =
    reconciliations
        |> Dict.values
        |> List.map (\r -> ( r.process, Rational.toFloatString r.qty ))


fromAllocations : Uuid -> List ( Uuid, RationalInput ) -> Dict String Reconciliation
fromAllocations event partialProcesses =
    partialProcesses
        |> List.foldl
            (\( process, sqty ) aggregate ->
                case Rational.fromString sqty of
                    Ok qty ->
                        let
                            r =
                                Reconciliation qty event process
                        in
                        Dict.insert (compare r) r aggregate

                    Err _ ->
                        aggregate
            )
            Dict.empty
