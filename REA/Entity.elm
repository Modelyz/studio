module REA.Entity exposing (Entity(..), decode, encode, toCommitment, toProcess)

import Json.Decode exposing (andThen)
import Json.Encode
import REA.Agent as A
import REA.Commitment as Cm
import REA.CommitmentType as CmT
import REA.Contract as C
import REA.ContractType as CT
import REA.Event as E
import REA.Process as P


type Entity
    = Process P.Process
    | Agent A.Agent
    | ContractType CT.ContractType
    | Contract C.Contract
      --    | ProcessType P.ProcessType
      --    | ResourceType RT.ResourceType
      --    | Resource R.Resource
      --    | EventType ET.EventType
    | Event E.Event
      --    | AgentType ET.AgentType
    | CommitmentType CmT.CommitmentType
    | Commitment Cm.Commitment


encode : Entity -> Json.Encode.Value
encode e =
    case e of
        Contract c ->
            C.encode c

        Agent c ->
            A.encode c

        ContractType c ->
            CT.encode c

        Process c ->
            P.encode c

        Commitment c ->
            Cm.encode c

        CommitmentType c ->
            CmT.encode c

        Event c ->
            E.encode c


decode : String -> Json.Decode.Decoder Entity
decode entityType =
    case entityType of
        "Process" ->
            Json.Decode.field "entity" P.decode
                |> andThen (\p -> Json.Decode.succeed <| Process p)

        "Agent" ->
            Json.Decode.field "entity" A.decode
                |> andThen (\a -> Json.Decode.succeed <| Agent a)

        "ContractType" ->
            Json.Decode.field "entity" CT.decode
                |> andThen (\ct -> Json.Decode.succeed <| ContractType ct)

        "Contract" ->
            Json.Decode.field "entity" C.decode
                |> andThen (\c -> Json.Decode.succeed <| Contract c)

        "Event" ->
            Json.Decode.field "entity" E.decode
                |> andThen (\e -> Json.Decode.succeed <| Event e)

        "CommitmentType" ->
            Json.Decode.field "entity" CmT.decode
                |> andThen (\ct -> Json.Decode.succeed <| CommitmentType ct)

        "Commitment" ->
            Json.Decode.field "entity" Cm.decode
                |> andThen (\c -> Json.Decode.succeed <| Commitment c)

        _ ->
            Json.Decode.fail "error decoding the entityType"


toProcess : Entity -> Maybe P.Process
toProcess entity =
    case entity of
        Process p ->
            Just p

        _ ->
            Nothing


toCommitment : Entity -> Maybe Cm.Commitment
toCommitment entity =
    case entity of
        Commitment c ->
            Just c

        _ ->
            Nothing
