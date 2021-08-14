module Page.Navbar exposing (view)

import ES
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href)
import DictSet
import Msg exposing (Msg(..))
import Route exposing (Route(..))


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
        [ div
            [ class "navbar-brand"
            ]
            [ a
                [ classList
                    [ ( "navbar-item", True )
                    , ( "active", isActive model Route.ProcessTypes )
                    ]
                , href "/process-types"
                ]
                [ text "Process Types"
                ]
            , div
                [ classList
                    [ ( "navbar-item", True )
                    , ( "has-dropdown", DictSet.size model.processTypes > 0)
                    , ( "active", isActive model <| Route.Processes Nothing)
                    , ("is-hoverable", True)
                    ]
                , href "#"
                ]
                [ a [class "navbar-item"]
                    <| [text "Processes"]
                         ++ case DictSet.size model.processTypes > 0 of
                            True -> [ div [ class "navbar-dropdown" ]
                                (model.processTypes
                                    |> DictSet.toList
                                    |> List.map (\pt -> a [class "navbar-item", href <| "/processes?type=" ++ pt.name] [text pt.name])
                                )]
                            False -> []
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
        ]
