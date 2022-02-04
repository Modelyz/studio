module Status exposing (ESStatus(..), WSStatus(..), esstatus2text, wsstatus2text)


type ESStatus
    = ESIdle
    | ESReading
    | ESReadFailed String
    | ESStoring


type WSStatus
    = WSOnline -- readyState 1: OPEN
    | WSOffline -- readyState 3: CLOSED
    | WSDisconnecting -- readyState 2: CLOSING
    | WSConnecting -- readyState 0: CONNECTING
    | WSSendFailed String
    | WSReceiveFailed String
    | WSReceiving -- for instance, reading the ES from the MS through WS
    | WSInit
    | WSUnexpected


esstatus2text : ESStatus -> String
esstatus2text status =
    case status of
        ESReading ->
            "ESReading"

        ESReadFailed err ->
            "ESReadFailed: " ++ err

        ESIdle ->
            "ESIdle"

        ESStoring ->
            "ESStoring"


wsstatus2text : WSStatus -> String
wsstatus2text status =
    case status of
        WSOnline ->
            "WSOnline"

        WSOffline ->
            "WSOffline: "

        WSSendFailed err ->
            "WSSendFailed: " ++ err

        WSDisconnecting ->
            "WSDisconnecting"

        WSInit ->
            "WSInit"

        WSUnexpected ->
            "WSUnexpected"

        WSReceiving ->
            "WSReceiving"

        WSReceiveFailed err ->
            "WSReceiveFailed: " ++ err

        WSConnecting ->
            "WSConnecting"
