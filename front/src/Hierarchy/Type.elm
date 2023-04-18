module Hierarchy.Type exposing (Type(..), all, compare, decoder, encode, fromString, toDisplay, toPluralDisplay, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


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


toDisplay : Type -> String
toDisplay t =
    case t of
        ResourceType ->
            "Resource Type"

        EventType ->
            "Event Type"

        AgentType ->
            "Agent Type"

        CommitmentType ->
            "Commitment Type"

        ContractType ->
            "Contract Type"

        ProcessType ->
            "Process Type"

        GroupType ->
            "Group Type"


toString : Type -> String
toString t =
    toDisplay t |> String.split " " |> String.join ""


compare : Type -> String
compare =
    toString


encode : Type -> Encode.Value
encode =
    toString >> Encode.string


decoder : Decoder Type
decoder =
    Decode.string |> Decode.andThen (fromString >> Maybe.map Decode.succeed >> Maybe.withDefault (Decode.fail "Unknown Type"))


toPluralDisplay : Type -> String
toPluralDisplay t =
    -- TODO not i18n friendly
    toDisplay t ++ "s"


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
