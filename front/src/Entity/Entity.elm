module Entity.Entity exposing (Entity(..), compare, decoder, encode)

import Agent.Agent as Agent exposing (Agent)
import AgentType.AgentType as AgentType exposing (AgentType)
import Commitment.Commitment as Commitment exposing (Commitment)
import CommitmentType.CommitmentType as CommitmentType exposing (CommitmentType)
import Contract.Contract as Contract exposing (Contract)
import ContractType.ContractType as ContractType exposing (ContractType)
import Event.Event as Event exposing (Event)
import EventType.EventType as EventType exposing (EventType)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process as Process exposing (Process)
import ProcessType.ProcessType as ProcessType exposing (ProcessType)
import Resource.Resource as Resource exposing (Resource)
import ResourceType.ResourceType as ResourceType exposing (ResourceType)


type
    Entity
    -- TODO Why an entity ? for AddPage and ListPage? Identifier ?
    = R Resource
    | E Event
    | A Agent
    | Cm Commitment
    | Cn Contract
    | P Process
    | RT ResourceType
    | ET EventType
    | AT AgentType
    | CmT CommitmentType
    | CnT ContractType
    | PT ProcessType


encode : Entity -> Value
encode entity =
    case entity of
        R r ->
            Encode.object
                [ ( "what", Encode.string "Resource" )
                , ( "value", Resource.encode r )
                ]

        E e ->
            Encode.object
                [ ( "what", Encode.string "Event" )
                , ( "value", Event.encode e )
                ]

        A a ->
            Encode.object
                [ ( "what", Encode.string "Agent" )
                , ( "value", Agent.encode a )
                ]

        Cm cm ->
            Encode.object
                [ ( "what", Encode.string "Commitment" )
                , ( "value", Commitment.encode cm )
                ]

        Cn cn ->
            Encode.object
                [ ( "what", Encode.string "Contract" )
                , ( "value", Contract.encode cn )
                ]

        P p ->
            Encode.object
                [ ( "what", Encode.string "Process" )
                , ( "value", Process.encode p )
                ]

        RT rt ->
            Encode.object
                [ ( "what", Encode.string "ResourceType" )
                , ( "value", ResourceType.encode rt )
                ]

        ET et ->
            Encode.object
                [ ( "what", Encode.string "EventType" )
                , ( "value", EventType.encode et )
                ]

        AT at ->
            Encode.object
                [ ( "what", Encode.string "AgentType" )
                , ( "value", AgentType.encode at )
                ]

        CmT cmt ->
            Encode.object
                [ ( "what", Encode.string "CommitmentType" )
                , ( "value", CommitmentType.encode cmt )
                ]

        CnT cnt ->
            Encode.object
                [ ( "what", Encode.string "ContractType" )
                , ( "value", ContractType.encode cnt )
                ]

        PT pt ->
            Encode.object
                [ ( "what", Encode.string "ProcessType" )
                , ( "value", ProcessType.encode pt )
                ]


decoder : Decoder Entity
decoder =
    Decode.field "what" Decode.string
        |> Decode.andThen
            (\t ->
                Decode.field "value"
                    (case t of
                        "Resource" ->
                            Decode.map R Resource.decoder

                        "Event" ->
                            Decode.map E Event.decoder

                        "Agent" ->
                            Decode.map A Agent.decoder

                        "Commitment" ->
                            Decode.map Cm Commitment.decoder

                        "Contract" ->
                            Decode.map Cn Contract.decoder

                        "Process" ->
                            Decode.map P Process.decoder

                        "ResourceType" ->
                            Decode.map RT ResourceType.decoder

                        "EventType" ->
                            Decode.map ET EventType.decoder

                        "AgentType" ->
                            Decode.map AT AgentType.decoder

                        "CommitmentType" ->
                            Decode.map CmT CommitmentType.decoder

                        "ContractType" ->
                            Decode.map CnT ContractType.decoder

                        "ProcessType" ->
                            Decode.map PT ProcessType.decoder

                        _ ->
                            Decode.fail "Unknown entity"
                    )
            )


toUuid : Entity -> Uuid
toUuid e =
    case e of
        R r ->
            r.uuid

        E ev ->
            ev.uuid

        A a ->
            a.uuid

        Cm cm ->
            cm.uuid

        Cn cn ->
            cn.uuid

        P p ->
            p.uuid

        RT rt ->
            rt.uuid

        ET et ->
            et.uuid

        AT at ->
            at.uuid

        CmT cmt ->
            cmt.uuid

        CnT cnt ->
            cnt.uuid

        PT pt ->
            pt.uuid


toUuidString : Entity -> String
toUuidString =
    toUuid >> Uuid.toString


compare : Entity -> String
compare =
    toUuidString
