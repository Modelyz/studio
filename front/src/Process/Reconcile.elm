module Process.Reconcile exposing (Reconciliation, byEvent, byProcess, compare, decoder, encode, fromPartialEvents, fromPartialProcesses, toPartialEvents, toPartialProcesses)

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


byEvent : Uuid -> Dict String Reconciliation -> Dict String Reconciliation
byEvent uuid =
    Dict.filter (\_ r -> r.event == uuid)


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


toPartialProcesses : Dict String Reconciliation -> List ( Uuid, RationalInput )
toPartialProcesses reconciliations =
    reconciliations
        |> Dict.values
        |> List.map (\r -> ( r.process, Rational.toFloatString r.qty ))


fromPartialProcesses : Uuid -> List ( Uuid, RationalInput ) -> Dict String Reconciliation
fromPartialProcesses event partialProcesses =
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


toPartialEvents : Dict String Reconciliation -> List ( Uuid, RationalInput )
toPartialEvents reconciliations =
    -- needed because Reconciliation does not allow failed rationals
    reconciliations
        |> Dict.values
        |> List.map (\r -> ( r.event, Rational.toFloatString r.qty ))


fromPartialEvents : Uuid -> List ( Uuid, RationalInput ) -> Dict String Reconciliation
fromPartialEvents process partialEvents =
    partialEvents
        |> List.foldl
            (\( event, sqty ) aggregate ->
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
