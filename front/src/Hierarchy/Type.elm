module Hierarchy.Type exposing (Type(..), all, fromString, toPluralString, toString)


type
    Type
    -- what types are identifiable?
    = ResourceType
    | EventType
    | AgentType
    | CommitmentType
    | ContractType
    | ProcessType
    | GroupType


all : List Type
all =
    [ ResourceType, EventType, AgentType, CommitmentType, ContractType, ProcessType, GroupType ]


toString : Type -> String
toString t =
    case t of
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


toPluralString : Type -> String
toPluralString t =
    -- TODO not i18n friendly
    toString t ++ "s"


fromString : String -> Maybe Type
fromString s =
    case s of
        "ResourceType" ->
            Just ResourceType

        "EventType" ->
            Just EventType

        "AgentType" ->
            Just AgentType

        "CommitmentType" ->
            Just CommitmentType

        "ContractType" ->
            Just ContractType

        "ProcessType" ->
            Just ProcessType

        "GroupType" ->
            Just GroupType

        _ ->
            Nothing
