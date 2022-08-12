module Entity.Entity exposing (Entity(..), compare, decoder, encode, findEntity, fromUuid, isChildOf, isChildOfAny, isParentOf, only, onlyTypes, toPluralString, toString, toType, toTypeUuid, toUuid, toUuidString)

import Agent.Agent as Agent exposing (Agent)
import AgentType.AgentType as AgentType exposing (AgentType)
import Commitment.Commitment as Commitment exposing (Commitment)
import CommitmentType.CommitmentType as CommitmentType exposing (CommitmentType)
import Contract.Contract as Contract exposing (Contract)
import ContractType.ContractType as ContractType exposing (ContractType)
import Dict exposing (Dict)
import Entity.Type as EntityType
import Event.Event as Event exposing (Event)
import EventType.EventType as EventType exposing (EventType)
import Group.Group as Group exposing (Group)
import GroupType.GroupType as GroupType exposing (GroupType)
import Item.Item as Item exposing (Item)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process as Process exposing (Process)
import ProcessType.ProcessType as ProcessType exposing (ProcessType)
import Resource.Resource as Resource exposing (Resource)
import ResourceType.ResourceType as ResourceType exposing (ResourceType)
import Time exposing (posixToMillis)
import Type exposing (Type)


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


fromUuid : Dict String Entity -> Uuid -> Maybe Entity
fromUuid allEntities uuid =
    Dict.filter (\_ e -> toUuid e == uuid) allEntities |> Dict.values |> List.head


only : Type -> Dict String (Item a) -> Dict String (Item a)
only t es =
    -- only of a certain constructor
    Dict.filter (\_ e -> e.what == t) es


onlyTypes : Dict String Entity -> Dict String Entity
onlyTypes es =
    Dict.filter (\_ e -> toType e |> EntityType.isType) es


findEntity : Uuid -> Dict String Entity -> Maybe Entity
findEntity uuid es =
    Dict.filter (\_ e -> toUuid e == uuid) es
        |> Dict.values
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

        RT rt ->
            rt.parent

        ET et ->
            et.parent

        AT at ->
            at.parent

        CmT ct ->
            ct.parent

        CnT ct ->
            ct.parent

        PT pt ->
            pt.parent


toType : Entity -> EntityType.Type
toType e =
    case e of
        R _ ->
            EntityType.Resource

        E _ ->
            EntityType.Event

        A _ ->
            EntityType.Agent

        Cm _ ->
            EntityType.Commitment

        Cn _ ->
            EntityType.Contract

        P _ ->
            EntityType.Process

        PT _ ->
            EntityType.Process

        RT _ ->
            EntityType.ResourceType

        ET _ ->
            EntityType.EventType

        AT _ ->
            EntityType.AgentType

        CmT _ ->
            EntityType.CommitmentType

        CnT _ ->
            EntityType.ContractType


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


toString : Entity -> String
toString =
    toType >> EntityType.toString


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

        PT _ ->
            "Proces Types"


toUuidString : Entity -> String
toUuidString =
    toUuid >> Uuid.toString


compare : Entity -> String
compare =
    toUuidString


isChildOfAny : Dict String Entity -> Dict String Entity -> Entity -> Bool
isChildOfAny entities es e =
    -- True if e is a child of one of the es (given the catalog of entities)
    es |> Dict.values |> List.any (isParentOf e entities)


isParentOf : Entity -> Dict String Entity -> Entity -> Bool
isParentOf child entities item =
    -- equality is considered parent
    if child == item then
        True

    else
        toTypeUuid child
            |> Maybe.andThen (\e -> findEntity e entities)
            |> Maybe.map (\x -> isParentOf x entities item)
            |> Maybe.withDefault False


isChildOf : Entity -> Dict String Entity -> Entity -> Bool
isChildOf parent entities item =
    -- true if item is a child of parent
    isParentOf item entities parent
