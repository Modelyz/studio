module View.Navbar exposing (view)

import DictSet as Set
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes as Attr
import IOStatus as IO exposing (toText)
import Route exposing (Route(..), firstSegment, toString)
import Shared
import Time exposing (posixToMillis)
import View exposing (color, itemHoverstyle, navbarHoverstyle, separator, shadowStyle)
import Websocket as WS exposing (toText)


type alias Model a =
    { a | route : Route }


view : Shared.Model -> Model a -> Element msg
view s m =
    column
        [ width (px 250), padding 10, alignTop, height fill, Font.color color.navbar.text, Background.color color.navbar.background ]
    <|
        List.intersperse
            (separator color.navbar.separator)
        <|
            [ navlink s m Home "Home"
            , navlink s m ProcessTypes "Process Types"
            , navlink s m ResourceTypes "Resource Types"
            , navlink s m AgentTypes "Agent Types"
            , navlink s m ContractTypes "Contract Types"
            , navlink s m EventTypes "Event Types"
            , navlink s m AgentTypes "Agent Types"
            , navlink s m CommitmentTypes "Commitment Types"
            , navlink s m ContractTypes "Contract Types"
            , navlink s m Groups "Groups"
            , navlink s m Identifiers "Identifiers"
            ]
                ++ (if Set.size s.state.processTypes > 0 then
                        s.state.processTypes
                            |> Set.toList
                            |> List.map (\pt -> navlink s m (ProcessType pt.name) pt.name)

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
                   , el [ Font.size 15, htmlAttribute <| Attr.id "msgs" ] (text <| "Route=" ++ toString m.route)
                   ]


navlink : Shared.Model -> Model a -> Route -> String -> Element msg
navlink s m r label =
    let
        active =
            firstSegment m.route == firstSegment r
    in
    link
        ([ Font.size 15
         , padding 10
         , width fill
         , mouseOver navbarHoverstyle
         ]
            ++ (if active then
                    [ Background.color color.navbar.hover ]

                else
                    []
               )
        )
        { url = toString r, label = text label }
