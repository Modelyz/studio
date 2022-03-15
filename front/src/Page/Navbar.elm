module Page.Navbar exposing (view)

import DictSet as Set
import Html exposing (Html, a, div, nav, span, text)
import Html.Attributes exposing (attribute, class, classList, href, id)
import IOStatus as IO exposing (toText)
import Msg exposing (Msg(..))
import Route exposing (Route(..))
import State exposing (State)
import Time exposing (posixToMillis)
import Websocket as WS exposing (toText)


type alias Model =
    State


isActive : Model -> Route -> Bool
isActive model route =
    model.route == route


view : Model -> Html Msg
view model =
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
                            , ( "active", isActive model Route.ProcessTypes )
                            ]
                        , href "/process-types"
                        ]
                        [ text "Process Types"
                        ]
                    , a
                        [ classList
                            [ ( "navbar-item", True )
                            , ( "active", isActive model Route.EventTypes )
                            ]
                        , href "/event-types"
                        ]
                        [ text "Event Types"
                        ]
                    , a
                        [ classList
                            [ ( "navbar-item", True )
                            , ( "active", isActive model Route.CommitmentTypes )
                            ]
                        , href "/commitment-types"
                        ]
                        [ text "Commitment Types"
                        ]
                    ]
                ]
            ]
        ]
            ++ (if Set.size model.processTypes > 0 then
                    model.processTypes
                        |> Set.toList
                        |> List.map (\pt -> a [ class "navbar-item", href <| "/processes?type=" ++ pt.name ] [ text pt.name ])

                else
                    []
               )
            ++ [ div [ class "navbar-item", id "WSStatus" ] [ text <| "WS=" ++ WS.toText model.wsstatus ]
               , div [ class "navbar-item", id "IOStatus" ] [ text <| "IO=" ++ IO.toText model.iostatus ]
               , div [ class "navbar-item", id "LastEvenTime" ] [ text <| "LastEvenTime=" ++ (String.fromInt <| posixToMillis model.lastEventTime) ]
               , div [ class "navbar-item", id "timeoutReconnect" ] [ text <| "timeoutReconnect=" ++ (String.fromInt <| model.timeoutReconnect) ]
               , div [ class "navbar-item", id "pending" ] [ text <| "pending=" ++ (String.fromInt <| Set.size model.pendingEvents) ]
               , div [ class "navbar-item", id "msgs" ] [ text <| "msgs=" ++ (String.fromInt <| Set.size model.uuids) ]
               ]
