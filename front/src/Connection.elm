module Connection exposing (Connection)

import Dict exposing (Dict)
import MessageId exposing (MessageId)
import Time


type alias Connection =
    { lastMessageTime : Time.Posix
    , uuids : Dict String MessageId
    }
