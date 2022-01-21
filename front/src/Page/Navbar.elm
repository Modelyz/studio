module Page.Navbar exposing (view)

import DictSet
import ES
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import Route exposing (Route(..))
import Status exposing (wsstatus2text)
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
        , button [ onClick InitiateConnection, class "button" ] [ text "Cnx" ] -- TODO remove and replace with an automatic connection
        , div [ class "navbar-item" ] [ text <| "WSStatus=" ++ wsstatus2text model.wsstatus ]
        , div [ class "navbar-item" ] [ text <| "LastEvenTime=" ++ (String.fromInt <| posixToMillis model.lastEventTime) ]
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