module REA.Entity exposing (..)

import Json.Encode
import REA
import REA.ContractType
import REA.Contract
import REA.Agent
import REA.Process
import REA.CommitmentType
import REA.Event


encode : REA.Entity -> Json.Encode.Value
encode e =
    case e of
        REA.CONTRACT c -> REA.Contract.encode c
        REA.AGENT c -> REA.Agent.encode c
        REA.CONTRACTTYPE c -> REA.ContractType.encode c
        REA.PROCESS c -> REA.Process.encode c
        REA.COMMITMENTTYPE c -> REA.CommitmentType.encode c
        REA.EVENT c -> REA.Event.encode c
