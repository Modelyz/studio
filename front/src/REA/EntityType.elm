module REA.EntityType exposing (EntityType(..), decoder, encode, toPluralString, toString)

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


type EntityType
    = ResourceType
    | EventType
    | AgentType
    | CommitmentType
    | ContractType
    | ProcessType


encode : EntityType -> Value
encode =
    toString >> Json.Encode.string


decoder : Decoder EntityType
decoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (fromString
                >> Maybe.map Json.Decode.succeed
                >> Maybe.withDefault (Json.Decode.fail "Unknown entity")
            )


fromString : String -> Maybe EntityType
fromString s =
    case s of
        "ProcessType" ->
            Just ProcessType

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

        _ ->
            Nothing


toString : EntityType -> String
toString e =
    case e of
        ProcessType ->
            "ProcessType"

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


toPluralString : EntityType -> String
toPluralString e =
    case e of
        ProcessType ->
            "ProcessTypes"

        ResourceType ->
            "ResourceTypes"

        EventType ->
            "EventTypes"

        AgentType ->
            "AgentTypes"

        CommitmentType ->
            "CommitmentTypes"

        ContractType ->
            "ContractTypes"
