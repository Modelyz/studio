module Status exposing (ESStatus(..), WSStatus(..), wsstatus2text)


type ESStatus
    = ESLoading
    | ESReadFailed String
    | ESLoaded


type WSStatus
    = WSIdle
    | WSSendFailed String
    | WSReceiveFailed String
    | WSLoading -- for instance, reading the ES from the MS through WS
    | WSReceiving


wsstatus2text : WSStatus -> String
wsstatus2text status =
    case status of
        WSIdle ->
            "idle"

        WSSendFailed s ->
            "send failed" ++ s

        WSLoading ->
            "loading"

        WSReceiving ->
            "receiving"

        WSReceiveFailed err ->
            "receive failed: " ++ err
