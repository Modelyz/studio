module Event exposing (..)

import Resource exposing (..)
import Agent exposing (..)

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


