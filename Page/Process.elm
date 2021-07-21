module Page.Process exposing (Model, view)

import Browser exposing (Document)
import DictSet as Set
import ES exposing (getCommitments)
import Html exposing (Html, a, br, div, img, nav, span, text)
import Html.Attributes exposing (attribute, class, href, src, width)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import Prng.Uuid as Uuid
import REA.Commitment as C exposing (Commitment)
import REA.Process exposing (Process)
import Status exposing (Status(..))


type alias Model =
    ES.State


viewNavbar : Html Msg
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


view : Model -> Process -> Document Msg
view model process =
    { title = "Process"
    , body =
        [ viewNavbar
        , viewContent model process
        ]
    }


viewContent : Model -> Process -> Html Msg
viewContent model process =
    div []
        [ div [ class "section", class "hscroll-container" ]
            [ span [] [ text <| "Pizza sale # " ++ Uuid.toString process.uuid ] ]
        , div
            [ class "section", class "hscroll-container" ]
            [ div [ class "button", class "hscroll", onClick <| NewCommitment process ] [ text "Order Pizza" ]
            , div [ class "button", class "hscroll", onClick <| NewCommitment process ] [ text "Ask payment" ]
            , div [ class "button", class "hscroll", onClick <| NewEvent process ] [ text "Receive Cash" ]
            , div [ class "button", class "hscroll", onClick <| NewEvent process ] [ text "Deliver Pizza" ]
            ]
        , div [ class "columns is-multiline" ]
            (getCommitments model process
                |> Set.toList
                |> List.sortBy C.compare
                |> List.reverse
                |> List.map viewThumbnail
            )
        ]


viewThumbnail : Commitment -> Html Msg
viewThumbnail c =
    div [ class "column is-one-quarter" ]
        [ a [ href <| "/commitment/" ++ Uuid.toString c.uuid ]
            [ div [ class "box" ]
                [ text <| c.name
                , br [] []
                , text <| Uuid.toString c.uuid
                ]
            ]
        ]
