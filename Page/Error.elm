module Page.Error exposing (view)

import Browser
import Html exposing (Html, a, div, img, nav, span, text)
import Html.Attributes exposing (attribute, class, href, src, width)


viewContent : Html.Html msg
viewContent =
    div [] [ text "Error" ]


viewNavbar : Html msg
viewNavbar =
    nav
        [ class "navbar"
        , attribute "role" "navigation"
        , attribute "aria-label" "main navigation"
        ]
        [ div
            [ class "navbar-brand"
            ]
            [ a
                [ class "navbar-item"
                , href "/"
                ]
                [ img
                    [ src "/static/logo.svg"
                    , width 50
                    ]
                    []
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


view : Browser.Document msg
view =
    { title = "Error"
    , body =
        [ viewNavbar
        , viewContent
        ]
    }
