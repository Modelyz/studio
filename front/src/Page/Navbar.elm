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
import View exposing (color, itemHoverstyle, navbarHoverstyle, shadowStyle)
import Websocket as WS exposing (toText)


view : Shared.Model -> Element msg
view s =
    column
        [ width (px 250), padding 10, alignTop, height fill, Font.color color.navbar.text, Background.color color.navbar.background ]
    <|
        List.intersperse
            separator
        <|
            [ navlink "/" "Home"
            , navlink "/process-types" "Process Types"
            , navlink "/event-types" "Event Types"
            , navlink "/commitment-types" "Commitment Types"
            ]
                ++ (if Set.size s.state.processTypes > 0 then
                        s.state.processTypes
                            |> Set.toList
                            |> List.map (\pt -> navlink ("/processes?type=" ++ pt.name) pt.name)

                    else
                        []
                   )
                ++ [ column [ alignBottom ]
                        [ separator
                        , el [ paddingXY 0 5, htmlAttribute <| Attr.id "WSStatus" ] (text <| "WS=" ++ WS.toText s.wsstatus)
                        , el [ htmlAttribute <| Attr.id "IOStatus" ] (text <| "IO=" ++ IO.toText s.iostatus)
                        , el [ htmlAttribute <| Attr.id "LastEvenTime" ] (text <| "LastEvenTime=" ++ (String.fromInt <| posixToMillis s.state.lastEventTime))
                        , el [ htmlAttribute <| Attr.id "timeoutReconnect" ] (text <| "timeoutReconnect=" ++ (String.fromInt <| s.timeoutReconnect))
                        , el [ htmlAttribute <| Attr.id "pending" ] (text <| "pending=" ++ (String.fromInt <| Set.size s.state.pendingEvents))
                        , el [ htmlAttribute <| Attr.id "msgs" ] (text <| "msgs=" ++ (String.fromInt <| Set.size s.state.uuids))
                        ]
                   ]


navlink : String -> String -> Element msg
navlink url label =
    link [ padding 10, width fill, mouseOver navbarHoverstyle ]
        { url = url, label = text label }


separator =
    row [ width fill, Border.width 1, Border.color color.navbar.separator ] []
