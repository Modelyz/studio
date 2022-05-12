module View.Navbar exposing (view)

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
import View exposing (color, itemHoverstyle, navbarHoverstyle, separator, shadowStyle)
import Websocket as WS exposing (toText)


view : Shared.Model -> Element msg
view s =
    column
        [ width (px 250), padding 10, alignTop, height fill, Font.color color.navbar.text, Background.color color.navbar.background ]
    <|
        List.intersperse
            (separator color.navbar.separator)
        <|
            [ navlink "/" "Home"
            , navlink "/process-types" "Process Types"
            , navlink "/resource-types" "Resource Types"
            , navlink "/event-types" "Event Types"
            , navlink "/agent-types" "Agent Types"
            , navlink "/commitment-types" "Commitment Types"
            , navlink "/contract-types" "Contract Types"
            , navlink "/groups" "Groups"
            , navlink "/identifiers" "Identifiers"
            ]
                ++ (if Set.size s.state.processTypes > 0 then
                        s.state.processTypes
                            |> Set.toList
                            |> List.map (\pt -> navlink ("/processes?type=" ++ pt.name) pt.name)

                    else
                        []
                   )
                ++ [ column [ spacing 5, alignBottom ]
                        [ row [ centerX, Font.color color.navbar.separator, padding 10, Font.size 15, Font.bold ] [ text "Studio" ]
                        , separator color.navbar.separator
                        , el [ paddingXY 0 5, htmlAttribute <| Attr.title <| "WSStatus=" ++ WS.toText s.wsstatus ] (text <| "WS  " ++ WS.toEmoji s.wsstatus)
                        , el [ htmlAttribute <| Attr.title <| "IOStatus" ++ "IO=" ++ IO.toText s.iostatus ] (text <| "IO  " ++ WS.toEmoji s.wsstatus)
                        , el [ Font.size 15, htmlAttribute <| Attr.id "LastEvenTime" ] (text <| "LastEvenTime=" ++ (String.fromInt <| posixToMillis s.state.lastEventTime))
                        , el [ Font.size 15, htmlAttribute <| Attr.id "timeoutReconnect" ] (text <| "timeoutReconnect=" ++ (String.fromInt <| s.timeoutReconnect))
                        , el [ Font.size 15, htmlAttribute <| Attr.id "pending" ] (text <| "pending=" ++ (String.fromInt <| Set.size s.state.pendingEvents))
                        , el [ Font.size 15, htmlAttribute <| Attr.id "msgs" ] (text <| "msgs=" ++ (String.fromInt <| Set.size s.state.uuids))
                        ]
                   ]


navlink : String -> String -> Element msg
navlink url label =
    link [ Font.size 15, padding 10, width fill, mouseOver navbarHoverstyle ]
        { url = url, label = text label }
