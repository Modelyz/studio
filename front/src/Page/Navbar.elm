module Page.Navbar exposing (view)

import DictSet
import ES
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import Route exposing (Route(..))
import Status exposing (esstatus2text, wsstatus2text)
import Time exposing (posixToMillis)


type alias Model =
    ES.State


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
        [ div [ class "navbar-brand" ] <|
            [ div [ class "navbar-item", class "has-dropdown", class "is-hoverable" ]
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
                ++ (case DictSet.size model.processTypes > 0 of
                        True ->
                            model.processTypes
                                |> DictSet.toList
                                |> List.map (\pt -> a [ class "navbar-item", href <| "/processes?type=" ++ pt.name ] [ text pt.name ])

                        False ->
                            []
                   )
        , div [ class "navbar-item", id "WSStatus" ] [ text <| wsstatus2text model.wsstatus ]
        , div [ class "navbar-item", id "ESStatus" ] [ text <| esstatus2text model.esstatus ]
        , div [ class "navbar-item", id "LastEvenTime" ] [ text <| "LastEvenTime=" ++ (String.fromInt <| posixToMillis model.lastEventTime) ]
        , div [ class "navbar-item", id "timeoutReconnect" ] [ text <| "timeoutReconnect=" ++ (String.fromInt <| model.timeoutReconnect) ]
        , a
            [ attribute "role" "button"
            , class "navbar-burger"
            , attribute "aria-label" "menu"
            , attribute "aria-expanded" "false"
            , attribute "dat-target" "navBar"
            ]
            [ span [ attribute "aria-hidden" "true" ] []
            , span [ attribute "aria-hidden" "true" ] []
            , span [ attribute "aria-hidden" "true" ] []
            ]
        ]
