module Page.Processes exposing (Model, view)

import Browser exposing (Document)
import DictSet
import ES
import Html exposing (Html, a, br, button, div, i, img, nav, span, text)
import Html.Attributes exposing (attribute, class, href, src, width)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import Page.Loading as Loading
import Page.Navbar as Navbar
import Prng.Uuid as Uuid
import REA.Process as P exposing (Process)
import Status exposing (Status(..))


type alias Model =
    ES.State


view : Model -> Document Msg
view model =
    { title = "Processes"
    , body =
        [ Navbar.view model
        , Loading.wrapper model (viewContent model)
        ]
    }


viewContent : Model -> Html Msg
viewContent model =
    div
        [ class "section"
        ]
        [ button
            [ onClick NewProcess
            , class "button"
            ]
            [ text "New pizza sale"
            ]
        , div [ class "columns is-multiline" ]
            (DictSet.toList model.processes
                |> List.sortBy P.compare
                |> List.reverse
                |> List.map viewThumbnail
            )
        ]


viewThumbnail : Process -> Html Msg
viewThumbnail p =
    div [ class "column is-one-quarter" ]
        [ a [ href <| "/process/" ++ Uuid.toString p.uuid ]
            [ div [ class "box" ]
                [ text <| p.name
                , br [] []
                , text <| Uuid.toString p.uuid
                ]
            ]
        ]
