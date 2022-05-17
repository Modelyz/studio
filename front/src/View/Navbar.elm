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
import Style exposing (color, itemHoverstyle, navbarHoverstyle, shadowStyle)
import Time exposing (posixToMillis)
import View exposing (hamburger, separator)
import Websocket as WS exposing (toText)


type alias Model a =
    { a | route : Route }


view : String -> Shared.Model -> Route -> Element Shared.Msg
view title s =
    if s.menu == Style.Desktop then
        desktop s

    else
        mobile title s


mobile : String -> Shared.Model -> Route -> Element Shared.Msg
mobile title s r =
    column [ width fill, padding 0, Font.color color.navbar.text, Background.color color.navbar.background ] <|
        List.intersperse
            (separator color.navbar.separator)
        <|
            [ hamburger title s ]
                ++ (if s.menu == Style.MobileClosed then
                        []

                    else
                        [ navlink s r Home "Home"
                        , navlink s r ProcessTypes "Process Types"
                        , navlink s r ResourceTypes "Resource Types"
                        , navlink s r AgentTypes "Agent Types"
                        , navlink s r ContractTypes "Contract Types"
                        , navlink s r EventTypes "Event Types"
                        , navlink s r AgentTypes "Agent Types"
                        , navlink s r CommitmentTypes "Commitment Types"
                        , navlink s r ContractTypes "Contract Types"
                        , navlink s r Groups "Groups"
                        , navlink s r Identifiers "Identifiers"
                        ]
                            ++ (if Set.size s.state.processTypes > 0 then
                                    s.state.processTypes
                                        |> Set.toList
                                        |> List.map (\pt -> navlink s r (ProcessType pt.name) pt.name)

                                else
                                    []
                               )
                   )


desktop : Shared.Model -> Route -> Element msg
desktop s r =
    column
        [ width (px 250), padding 10, alignTop, height fill, Font.color color.navbar.text, Background.color color.navbar.background ]
    <|
        List.intersperse
            (separator color.navbar.separator)
        <|
            [ navlink s r Home "Home"
            , navlink s r ProcessTypes "Process Types"
            , navlink s r ResourceTypes "Resource Types"
            , navlink s r AgentTypes "Agent Types"
            , navlink s r ContractTypes "Contract Types"
            , navlink s r EventTypes "Event Types"
            , navlink s r AgentTypes "Agent Types"
            , navlink s r CommitmentTypes "Commitment Types"
            , navlink s r ContractTypes "Contract Types"
            , navlink s r Groups "Groups"
            , navlink s r Identifiers "Identifiers"
            ]
                ++ (if Set.size s.state.processTypes > 0 then
                        s.state.processTypes
                            |> Set.toList
                            |> List.map (\pt -> navlink s r (ProcessType pt.name) pt.name)

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
                   , el [ Font.size 15, htmlAttribute <| Attr.id "msgs" ] (text <| "Route=" ++ toString r)
                   ]


navlink : Shared.Model -> Route -> Route -> String -> Element msg
navlink s currentRoute linkRoute label =
    let
        active =
            currentRoute == linkRoute
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
        { url = toString linkRoute, label = text label }
