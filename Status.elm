module Status exposing (ESStatus(..), WSStatus(..))


type ESStatus
    = ESLoading
    | ESReadFailed String
    | ESLoaded


type WSStatus
    = WSIdle
    | WSSending
    | WSSent
    | WSAcknowledged
    | WSSendFailed String
    | WSLoading -- for instance, reading the ES from the MS through WS
    | WSConnecting
