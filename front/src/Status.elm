module Status exposing (ESStatus(..), WSStatus(..), esstatus2text, wsstatus2text)


type ESStatus
    = ESIdle
    | ESReading
    | ESReadFailed String
    | ESStoring


type WSStatus
    = WSIdle
    | WSSendFailed String
    | WSReceiveFailed String
    | WSLoading -- for instance, reading the ES from the MS through WS
    | WSConnecting


esstatus2text : ESStatus -> String
esstatus2text status =
    case status of
        ESReading ->
            "ESReading"

        ESReadFailed err ->
            "ESReadFailed " ++ err

        ESIdle ->
            "ESIdle"

        ESStoring ->
            "ESStoring"


wsstatus2text : WSStatus -> String
wsstatus2text status =
    case status of
        WSIdle ->
            "WSIdle"

        WSSendFailed s ->
            "WSSendFailed" ++ s

        WSLoading ->
            "WSLoading"

        WSReceiveFailed err ->
            "WSReceiveFailed" ++ err

        WSConnecting ->
            "WSConnecting"
