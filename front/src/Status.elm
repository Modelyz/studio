module Status exposing (ESStatus(..), WSStatus(..), esstatus2text, wsstatus2text)


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


esstatus2text : ESStatus -> String
esstatus2text status =
    case status of
        ESLoading ->
            "ESLoading"

        ESReadFailed err ->
            "ESReadFailed " ++ err

        ESLoaded ->
            "ESLoaded"


wsstatus2text : WSStatus -> String
wsstatus2text status =
    case status of
        WSIdle ->
            "WSIdle"

        WSSendFailed s ->
            "WSSendFailed" ++ s

        WSLoading ->
            "WSLoading"

        WSReceiving ->
            "WSReceiving"

        WSReceiveFailed err ->
            "WSReceiveFailed" ++ err
