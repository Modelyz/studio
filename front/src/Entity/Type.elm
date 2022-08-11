module Entity.Type exposing (Type(..), all, allStrings, decoder, encode, fromType, isType, toPluralString, toString, toType)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type
    Type
    -- TODO see if it's still useful
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


isType : Type -> Bool
isType t =
    (String.slice 0 4 <| String.reverse <| toString t) == "epyT"


toType : Type -> Maybe Type
toType t =
    -- TODO rename to be more explicit
    if isType t then
        Just t

    else
        fromString <| (\ts -> ts ++ "Type") <| toString t


fromType : Type -> Maybe Type
fromType t =
    -- TODO rename to be more explicit
    if isType t then
        fromString <| String.slice 0 -4 <| toString t

    else
        Just t


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


encode : Type -> Encode.Value
encode =
    toString >> Encode.string


decoder : Decoder Type
decoder =
    Decode.string |> Decode.andThen (fromString >> Maybe.map Decode.succeed >> Maybe.withDefault (Decode.fail "Unknown Type"))
