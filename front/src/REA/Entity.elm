module REA.Entity exposing (Entity(..), compare, decoder, encode, toPluralString, toString, toType, toUuid)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Prng.Uuid as Uuid exposing (Uuid)
import REA.Agent as A exposing (Agent)
import REA.Commitment as CM exposing (Commitment)
import REA.Contract as CN exposing (Contract)
import REA.Event as E exposing (Event)
import REA.Process as P exposing (Process)
import REA.Resource as R exposing (Resource)


type Entity
    = Resource Resource
    | Event Event
    | Agent Agent
    | Commitment Commitment
    | Contract Contract
    | Process Process


toType : Entity -> String
toType entity =
    case entity of
        Resource x ->
            x.type_

        Event x ->
            x.type_

        Agent x ->
            x.type_

        Commitment x ->
            x.type_

        Contract x ->
            x.type_

        Process x ->
            x.type_


encode : Entity -> Value
encode entity =
    case entity of
        Resource r ->
            Encode.object
                [ ( "what", Encode.string "Resource" )
                , ( "type", R.encode r )
                ]

        Event e ->
            Encode.object
                [ ( "what", Encode.string "Event" )
                , ( "type", E.encode e )
                ]

        Agent a ->
            Encode.object
                [ ( "what", Encode.string "Agent" )
                , ( "type", A.encode a )
                ]

        Commitment cm ->
            Encode.object
                [ ( "what", Encode.string "Commitment" )
                , ( "type", CM.encode cm )
                ]

        Contract cn ->
            Encode.object
                [ ( "what", Encode.string "Contract" )
                , ( "type", CN.encode cn )
                ]

        Process p ->
            Encode.object
                [ ( "what", Encode.string "Process" )
                , ( "type", P.encode p )
                ]


decoder : Decoder Entity
decoder =
    Decode.field "what" Decode.string
        |> Decode.andThen
            (\t ->
                Decode.field "type"
                    (case t of
                        "Resource" ->
                            Decode.map Resource R.decoder

                        "Event" ->
                            Decode.map Event E.decoder

                        "Agent" ->
                            Decode.map Agent A.decoder

                        "Commitment" ->
                            Decode.map Commitment CM.decoder

                        "Contract" ->
                            Decode.map Contract CN.decoder

                        "Process" ->
                            Decode.map Process P.decoder

                        _ ->
                            Decode.fail "Unknown entity"
                    )
            )


toUuid : Entity -> Uuid
toUuid e =
    case e of
        Process p ->
            p.uuid

        Resource r ->
            r.uuid

        Event ev ->
            ev.uuid

        Agent a ->
            a.uuid

        Commitment cm ->
            cm.uuid

        Contract cn ->
            cn.uuid


toString : Entity -> String
toString e =
    case e of
        Process p ->
            "Process " ++ Uuid.toString p.uuid

        Resource r ->
            "Resource " ++ Uuid.toString r.uuid

        Event ev ->
            "Event " ++ Uuid.toString ev.uuid

        Agent a ->
            "Agent " ++ Uuid.toString a.uuid

        Commitment cm ->
            "Commitment " ++ Uuid.toString cm.uuid

        Contract cn ->
            "Contract " ++ Uuid.toString cn.uuid


toPluralString : Entity -> String
toPluralString e =
    case e of
        Process _ ->
            "Processes"

        Resource _ ->
            "Resources"

        Event _ ->
            "Events"

        Agent _ ->
            "Agents"

        Commitment _ ->
            "Commitments"

        Contract _ ->
            "Contracts"


compare : Entity -> String
compare =
    toUuid >> Uuid.toString
