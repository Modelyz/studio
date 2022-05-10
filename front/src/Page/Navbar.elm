module Page.Navbar exposing (view)

import DictSet as Set
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes as Attr
import IOStatus as IO exposing (toText)
import Route exposing (Route(..))
import Shared
import Time exposing (posixToMillis)
import View exposing (color, itemHoverstyle, navbar, shadowStyle)
import Websocket as WS exposing (toText)


view : Shared.Model -> Element msg
view s =
    column
        [ padding 10, alignTop, height fill, Font.color color.navbar.text, Background.color color.navbar.background ]
    <|
        List.intersperse
            navbar.separator
        <|
            [ navbar.link
                { url = "/", label = text "Home" }
            , navbar.link
                { url = "/process-types", label = text "Process Types" }
            , navbar.link
                { url = "/event-types", label = text "Event Types" }
            , navbar.link
                { url = "/commitment-types", label = text "Commitment Types" }
            ]
                ++ (if Set.size s.state.processTypes > 0 then
                        s.state.processTypes
                            |> Set.toList
                            |> List.map (\pt -> navbar.link { url = "/processes?type=" ++ pt.name, label = text pt.name })

                    else
                        []
                   )
                ++ [ column [ alignBottom ]
                        [ navbar.separator
                        , paragraph [ paddingXY 0 5, htmlAttribute <| Attr.id "WSStatus" ] [ text <| "WS=" ++ WS.toText s.wsstatus ]
                        , paragraph [ htmlAttribute <| Attr.id "IOStatus" ] [ text <| "IO=" ++ IO.toText s.iostatus ]
                        , paragraph [ htmlAttribute <| Attr.id "LastEvenTime" ] [ text <| "LastEvenTime=" ++ (String.fromInt <| posixToMillis s.state.lastEventTime) ]
                        , paragraph [ htmlAttribute <| Attr.id "timeoutReconnect" ] [ text <| "timeoutReconnect=" ++ (String.fromInt <| s.timeoutReconnect) ]
                        , paragraph [ htmlAttribute <| Attr.id "pending" ] [ text <| "pending=" ++ (String.fromInt <| Set.size s.state.pendingEvents) ]
                        , paragraph [ htmlAttribute <| Attr.id "msgs" ] [ text <| "msgs=" ++ (String.fromInt <| Set.size s.state.uuids) ]
                        ]
                   ]
