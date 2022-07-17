module Navbar exposing (view)

import DictSet as Set
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Entity.Entity as Entity
import Html.Attributes as Attr
import IOStatus as IO exposing (toText)
import Route exposing (Route, firstSegment, toString)
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


links : Shared.Model -> Route -> List (Element Shared.Msg)
links s r =
    [ menuitem s r Route.Home "Home"
    , menuitem s r Route.ProcessTypeList "Process Types"
    , menuitem s r Route.ResourceTypeList "Resource Types"
    , menuitem s r Route.EventTypeList "Event Types"
    , menuitem s r Route.AgentTypeList "Agent Types"
    , menuitem s r Route.CommitmentTypeList "Commitment Types"
    , menuitem s r Route.ContractTypeList "Contract Types"
    , menuitem s r Route.GroupTypeList "Group Types"
    , menuitem s r Route.IdentifierTypeList "Identifiers"
    , menuitem s r Route.ResourceList "Resources"
    , menuitem s r Route.EventList "Events"
    , menuitem s r Route.AgentList "Agents"
    , menuitem s r Route.CommitmentList "Commitments"
    , menuitem s r Route.ContractList "Contracts"
    , menuitem s r Route.GroupList "Groups"
    , menuitem s r (Route.ProcessList Nothing) "Processes"
    ]
        ++ (let
                processTypes =
                    s.state.entities |> Entity.only "ProcessType"
            in
            if Set.size processTypes > 0 then
                processTypes
                    |> Set.toList
                    |> List.map
                        (\pt ->
                            let
                                name =
                                    Entity.toUuidString pt
                            in
                            menuitem s r (Route.ProcessTypeView name) name
                        )

            else
                []
           )


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
                        links s r
                   )


desktop : Shared.Model -> Route -> Element Shared.Msg
desktop s r =
    column
        [ width (px 250), padding 10, alignTop, height fill, Font.color color.navbar.text, Background.color color.navbar.background ]
    <|
        List.intersperse (separator color.navbar.separator) (links s r)
            ++ [ column [ spacing 5, alignBottom ]
                    [ row [ centerX, Font.color color.navbar.separator, padding 10, Font.size 15, Font.bold ] [ text "Studio" ]
                    , separator color.navbar.separator
                    , el [ paddingXY 0 5, htmlAttribute <| Attr.title <| "WSStatus=" ++ WS.toText s.wsstatus ] (text <| "WS  " ++ WS.toEmoji s.wsstatus)
                    , el [ htmlAttribute <| Attr.title <| "IOStatus" ++ "IO=" ++ IO.toText s.iostatus ] (text <| "IO  " ++ WS.toEmoji s.wsstatus)
                    , el [ Font.size 15, htmlAttribute <| Attr.id "LastMessageTime" ] (text <| "LastMessageTime=" ++ (String.fromInt <| posixToMillis s.state.lastMessageTime))
                    , el [ Font.size 15, htmlAttribute <| Attr.id "timeoutReconnect" ] (text <| "timeoutReconnect=" ++ (String.fromInt <| s.timeoutReconnect))
                    , el [ Font.size 15, htmlAttribute <| Attr.id "pending" ] (text <| "pending=" ++ (String.fromInt <| Set.size s.state.pendingMessages))
                    , el [ Font.size 15, htmlAttribute <| Attr.id "entities" ] (text <| "entities=" ++ (String.fromInt <| Set.size s.state.entities))
                    , el [ Font.size 15, htmlAttribute <| Attr.id "msgs" ] (text <| "msgs=" ++ (String.fromInt <| Set.size s.state.uuids))
                    , el [ Font.size 15, htmlAttribute <| Attr.id "msgs" ] (text <| "Route=" ++ toString r)
                    ]
               ]


menuitem : Shared.Model -> Route -> Route -> String -> Element msg
menuitem s currentRoute linkRoute label =
    let
        active =
            firstSegment currentRoute == firstSegment linkRoute
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
