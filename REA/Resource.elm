module REA.Resource exposing (..)

import Prng.Uuid

import ResourceType exposing (ResourceType)

type alias Resource =
    { name: String
    , uuid: Prng.Uuid.Uuid
    , rtype: ResourceType
    }

