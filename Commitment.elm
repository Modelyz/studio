module Commitment exposing (..)

import Agent exposing (..)
import Resource exposing (..)

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


