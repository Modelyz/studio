module Connection exposing (Connection)

import Dict exposing (Dict)
import Metadata exposing (Metadata)
import Time


type alias Connection =
    { lastMessageTime : Time.Posix
    , uuids : Dict String Metadata
    }
