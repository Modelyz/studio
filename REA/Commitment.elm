module REA.Commitment exposing (..)

import REA.Agent exposing (Agent)
import REA.Resource exposing (ResourceType)

type CommitmentType =
    CommitmentType
        { name: String
        , ctype: Maybe CommitmentType
         }


type Commitment =
    Commitment
        { name: String
        , ctype: CommitmentType
        , qty: Float
        , rtype: ResourceType
        , provider: Agent
        , receiver: Agent }


