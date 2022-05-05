module Page.Navbar exposing (view)

import DictSet as Set
import Html exposing (Html, a, div, nav, text)
import Html.Attributes exposing (attribute, class, classList, href, id)
import IOStatus as IO exposing (toText)
import Route exposing (Route(..))
import Shared
import Time exposing (posixToMillis)
import Websocket as WS exposing (toText)


view : Shared.Model -> Route.Route -> Html msg
view shared route =
    nav
        [ class "navbar"
        , attribute "role" "navigation"
        , attribute "aria-label" "main navigation"
        ]
    <|
        [ div
            [ class "navbar-item", class "is-hoverable" ]
            [ a [ class "navbar-item" ]
                [ text "Configuration"
                , div [ class "navbar-dropdown" ]
                    [ a
                        [ classList
                            [ ( "navbar-item", True )
                            , ( "active", route == Route.ProcessTypes )
                            ]
                        , href "/process-types"
                        ]
                        [ text "Process Types"
                        ]
                    , a
                        [ classList
                            [ ( "navbar-item", True )
                            , ( "active", route == Route.EventTypes )
                            ]
                        , href "/event-types"
                        ]
                        [ text "Event Types"
                        ]
                    , a
                        [ classList
                            [ ( "navbar-item", True )
                            , ( "active", route == Route.CommitmentTypes )
                            ]
                        , href "/commitment-types"
                        ]
                        [ text "Commitment Types"
                        ]
                    ]
                ]
            ]
        ]
            ++ (if Set.size shared.processTypes > 0 then
                    shared.processTypes
                        |> Set.toList
                        |> List.map (\pt -> a [ class "navbar-item", href <| "/processes?type=" ++ pt.name ] [ text pt.name ])

                else
                    []
               )
            ++ [ div [ class "navbar-item", id "WSStatus" ] [ text <| "WS=" ++ WS.toText shared.wsstatus ]
               , div [ class "navbar-item", id "IOStatus" ] [ text <| "IO=" ++ IO.toText shared.iostatus ]
               , div [ class "navbar-item", id "LastEvenTime" ] [ text <| "LastEvenTime=" ++ (String.fromInt <| posixToMillis shared.lastEventTime) ]
               , div [ class "navbar-item", id "timeoutReconnect" ] [ text <| "timeoutReconnect=" ++ (String.fromInt <| shared.timeoutReconnect) ]
               , div [ class "navbar-item", id "pending" ] [ text <| "pending=" ++ (String.fromInt <| Set.size shared.pendingEvents) ]
               , div [ class "navbar-item", id "msgs" ] [ text <| "msgs=" ++ (String.fromInt <| Set.size shared.uuids) ]
               ]
