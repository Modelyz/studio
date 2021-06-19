module REA.Event exposing (..)

import REA.Resource exposing (ResourceType)
import REA.Agent exposing (Agent)

type EventType =
    EventType
        { name: String
        , etype: Maybe EventType
         }


type Event =
    Event
        { name: String
        , etype: EventType
        , qty: Float
        , rtype: ResourceType
        , provider: Agent
        , receiver: Agent }


