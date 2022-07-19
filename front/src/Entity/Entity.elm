module Entity.Entity exposing (Entity(..), compare, decoder, encode, findEntity, fromUuid, isChildOf, isChildOfAny, isParentOf, only, toPluralString, toString, toType, toTypeUuid, toUuid, toUuidString)

import Agent.Agent as Agent exposing (Agent)
import AgentType.AgentType as AgentType exposing (AgentType)
import Commitment.Commitment as Commitment exposing (Commitment)
import CommitmentType.CommitmentType as CommitmentType exposing (CommitmentType)
import Contract.Contract as Contract exposing (Contract)
import ContractType.ContractType as ContractType exposing (ContractType)
import DictSet as Set exposing (DictSet)
import Entity.Type as Type exposing (Type)
import Event.Event as Event exposing (Event)
import EventType.EventType as EventType exposing (EventType)
import Group.Group as Group exposing (Group)
import GroupType.GroupType as GroupType exposing (GroupType)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process as Process exposing (Process)
import ProcessType.ProcessType as ProcessType exposing (ProcessType)
import Resource.Resource as Resource exposing (Resource)
import ResourceType.ResourceType as ResourceType exposing (ResourceType)
import Time exposing (posixToMillis)


type Entity
    = R Resource
    | E Event
    | A Agent
    | Cm Commitment
    | Cn Contract
    | P Process
    | G Group
    | RT ResourceType
    | ET EventType
    | AT AgentType
    | CmT CommitmentType
    | CnT ContractType
    | PT ProcessType
    | GT GroupType


fromUuid : DictSet String Entity -> Uuid -> Maybe Entity
fromUuid entities type_ =
    Set.filter (\e -> toUuid e == type_) entities |> Set.toList |> List.head


only : String -> DictSet String Entity -> DictSet String Entity
only t es =
    -- only of a certain constructor
    Set.filter (\e -> toString e == t) es


findEntity : Uuid -> DictSet String Entity -> Maybe Entity
findEntity uuid es =
    Set.filter (\e -> toUuid e == uuid) es
        |> Set.toList
        |> List.head


toTypeUuid : Entity -> Maybe Uuid
toTypeUuid entity =
    case entity of
        R r ->
            Just r.type_

        E e ->
            Just e.type_

        A a ->
            Just a.type_

        Cm c ->
            Just c.type_

        Cn c ->
            Just c.type_

        P p ->
            Just p.type_

        G g ->
            Just g.type_

        RT rt ->
            rt.type_

        ET et ->
            et.type_

        AT at ->
            at.type_

        CmT ct ->
            ct.type_

        CnT ct ->
            ct.type_

        PT pt ->
            pt.type_

        GT gt ->
            gt.type_


toType : Entity -> Type
toType e =
    case e of
        R _ ->
            Type.Resource

        E _ ->
            Type.Event

        A _ ->
            Type.Agent

        Cm _ ->
            Type.Commitment

        Cn _ ->
            Type.Contract

        G _ ->
            Type.Group

        P _ ->
            Type.Process

        PT _ ->
            Type.Process

        RT _ ->
            Type.ResourceType

        ET _ ->
            Type.EventType

        AT _ ->
            Type.AgentType

        CmT _ ->
            Type.CommitmentType

        CnT _ ->
            Type.ContractType

        GT _ ->
            Type.GroupType


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

        G g ->
            Encode.object
                [ ( "what", Encode.string "Group" )
                , ( "value", Group.encode g )
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

        GT gt ->
            Encode.object
                [ ( "what", Encode.string "GroupType" )
                , ( "value", GroupType.encode gt )
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

                        "Group" ->
                            Decode.map G Group.decoder

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

                        "GroupType" ->
                            Decode.map GT GroupType.decoder

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

        G g ->
            g.uuid

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

        GT gt ->
            gt.uuid

        PT pt ->
            pt.uuid


toString : Entity -> String
toString =
    toType >> Type.toString


toPluralString : Entity -> String
toPluralString e =
    case e of
        -- TODO remove at i18n time
        P _ ->
            "Processes"

        R _ ->
            "Resources"

        E _ ->
            "Events"

        A _ ->
            "Agents"

        Cm _ ->
            "Commitments"

        Cn _ ->
            "Contracts"

        G _ ->
            "Groups"

        RT _ ->
            "Resource Types"

        ET _ ->
            "Event Types"

        AT _ ->
            "Agent Types"

        CmT _ ->
            "Commitment Types"

        CnT _ ->
            "Contract Types"

        GT _ ->
            "Group Types"

        PT _ ->
            "Proces Types"


toUuidString : Entity -> String
toUuidString =
    toUuid >> Uuid.toString


compare : Entity -> String
compare =
    toUuidString


isChildOfAny : DictSet String Entity -> DictSet String Entity -> Entity -> Bool
isChildOfAny entities es e =
    -- True if e is a child of one of the es (given the catalog of entities)
    es |> Set.toList |> List.any (isParentOf e entities)


isParentOf : Entity -> DictSet String Entity -> Entity -> Bool
isParentOf child entities item =
    -- equality is considered parent
    if child == item then
        True

    else
        toTypeUuid child
            |> Maybe.andThen (\e -> findEntity e entities)
            |> Maybe.map (\x -> isParentOf x entities item)
            |> Maybe.withDefault False


isChildOf : Entity -> DictSet String Entity -> Entity -> Bool
isChildOf parent entities item =
    -- true if item is a child of parent
    isParentOf item entities parent
