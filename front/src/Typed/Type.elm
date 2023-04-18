module Typed.Type exposing (Type(..), all, decoder, encode, fromString, toDisplay, toHierarchic, toPluralDisplay, toString)

import Hierarchy.Type as HType
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


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


toDisplay : Type -> String
toDisplay =
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
    case t of
        Process ->
            "Processes"

        _ ->
            toDisplay t ++ "s"


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
