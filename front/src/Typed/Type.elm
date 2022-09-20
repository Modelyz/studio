module Typed.Type exposing (Type(..), all, fromHierarchic, fromString, toHierarchic, toPluralString, toString)

import Hierarchy.Type as HType


type
    Type
    -- what types are identifiable?
    = Resource
    | Event
    | Agent
    | Commitment
    | Contract
    | Process
    | Group


all : List Type
all =
    [ Resource, Event, Agent, Commitment, Contract, Process, Group ]


fromHierarchic : HType.Type -> Type
fromHierarchic ht =
    case ht of
        HType.ResourceType ->
            Resource

        HType.EventType ->
            Event

        HType.AgentType ->
            Agent

        HType.CommitmentType ->
            Commitment

        HType.ContractType ->
            Contract

        HType.ProcessType ->
            Process

        HType.GroupType ->
            Group


toHierarchic : Type -> HType.Type
toHierarchic t =
    case t of
        Resource ->
            HType.ResourceType

        Event ->
            HType.EventType

        Agent ->
            HType.AgentType

        Commitment ->
            HType.CommitmentType

        Contract ->
            HType.ContractType

        Process ->
            HType.ProcessType

        Group ->
            HType.GroupType


toString : Type -> String
toString t =
    case t of
        Resource ->
            "Resource"

        Event ->
            "Event"

        Agent ->
            "Agent"

        Commitment ->
            "Commitment"

        Contract ->
            "Contract"

        Process ->
            "Process"

        Group ->
            "Group"


toPluralString : Type -> String
toPluralString t =
    -- TODO not i18n friendly
    case t of
        Process ->
            "Processes"

        _ ->
            toString t ++ "s"


fromString : String -> Maybe Type
fromString s =
    case s of
        "Resource" ->
            Just Resource

        "Event" ->
            Just Event

        "Agent" ->
            Just Agent

        "Commitment" ->
            Just Commitment

        "Contract" ->
            Just Contract

        "Process" ->
            Just Process

        "Group" ->
            Just Group

        _ ->
            Nothing
