module REA.Entity exposing (..)

import Json.Decode exposing (andThen)
import Json.Encode

import REA
import REA.Agent
import REA.CommitmentType
import REA.Contract
import REA.ContractType
import REA.Event
import REA.Process


encode : REA.Entity -> Json.Encode.Value
encode e =
    case e of
        REA.CONTRACT c -> REA.Contract.encode c
        REA.AGENT c -> REA.Agent.encode c
        REA.CONTRACTTYPE c -> REA.ContractType.encode c
        REA.PROCESS c -> REA.Process.encode c
        REA.COMMITMENTTYPE c -> REA.CommitmentType.encode c
        REA.EVENT c -> REA.Event.encode c


decode : String -> Json.Decode.Decoder REA.Entity
decode entityType =
    case entityType of
        "Process" -> Json.Decode.field "entity" REA.Process.decode
            |> andThen REA.Process.entity
        "Agent" -> Json.Decode.field "entity" REA.Agent.decode
            |> andThen REA.Agent.entity
        "ContractType" -> Json.Decode.field "entity" REA.ContractType.decode
            |> andThen REA.ContractType.entity
        "Contract" -> Json.Decode.field "entity" REA.Contract.decode
            |> andThen REA.Contract.entity
        "Event" -> Json.Decode.field "entity" REA.Event.decode
            |> andThen REA.Event.entity
        "CommitmentType" -> Json.Decode.field "entity" REA.CommitmentType.decode
            |> andThen REA.CommitmentType.entity
        _ -> Json.Decode.fail "error decoding the entityType"

