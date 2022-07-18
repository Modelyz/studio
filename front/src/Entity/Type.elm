module Entity.Type exposing (Type(..), all, allStrings, toString)


type Type
    = Resource
    | Event
    | Agent
    | Commitment
    | Contract
    | Process
    | Group
    | ResourceType
    | EventType
    | AgentType
    | CommitmentType
    | ContractType
    | ProcessType
    | GroupType


all : List Type
all =
    [ ResourceType, EventType, AgentType, CommitmentType, ContractType, ProcessType, GroupType, Resource, Event, Agent, Commitment, Process, Group ]


allStrings : List String
allStrings =
    List.map toString all


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

        ResourceType ->
            "ResourceType"

        EventType ->
            "EventType"

        AgentType ->
            "AgentType"

        CommitmentType ->
            "CommitmentType"

        ContractType ->
            "ContractType"

        ProcessType ->
            "ProcessType"

        GroupType ->
            "GroupType"
