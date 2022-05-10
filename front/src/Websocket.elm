port module Websocket exposing (WSStatus(..), fromReadyState, toEmoji, toText, wsConnect, wsSend)

import Json.Decode as Decode exposing (decodeValue)



-- send events through WS


port wsSend : String -> Cmd msg


port wsConnect : () -> Cmd msg


type WSStatus
    = WSOpen -- readyState 1: OPEN
    | WSClosed -- readyState 3: CLOSED
    | WSClosing -- readyState 2: CLOSING
    | WSConnecting -- readyState 0: CONNECTING


toText : WSStatus -> String
toText status =
    -- https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/readyState
    case status of
        WSOpen ->
            "WSOpen"

        WSClosed ->
            "WSClosed"

        WSClosing ->
            "WSClosing"

        WSConnecting ->
            "WSConnecting"


toEmoji : WSStatus -> String
toEmoji status =
    -- https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/readyState
    case status of
        WSOpen ->
            "🔵"

        WSClosed ->
            "🔴"

        WSClosing ->
            "🔶"

        WSConnecting ->
            "🔷"


fromReadyState : Decode.Value -> WSStatus
fromReadyState value =
    let
        s =
            decodeValue Decode.int value
                |> Result.withDefault 9
    in
    case s of
        0 ->
            WSConnecting

        1 ->
            WSOpen

        2 ->
            WSClosing

        3 ->
            WSClosed

        _ ->
            WSClosed
