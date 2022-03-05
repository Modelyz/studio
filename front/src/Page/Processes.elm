module Page.Processes exposing (Model, view)

import Browser exposing (Document)
import DictSet
import ES
import Html exposing (Html, a, br, button, div, i, img, nav, span, text)
import Html.Attributes exposing (attribute, class, href, id, src, width)
import Html.Events exposing (onClick)
import IOStatus exposing (IOStatus(..))
import Msg exposing (Msg(..))
import Page.Navbar as Navbar
import Prng.Uuid as Uuid
import REA.Process as P exposing (Process)
import REA.ProcessType exposing (ProcessType)


type alias Model =
    ES.State


view : Model -> ProcessType -> Document Msg
view model ptype =
    { title = "Processes"
    , body =
        [ Navbar.view model
        , viewContent model ptype
        ]
    }


viewContent : Model -> ProcessType -> Html Msg
viewContent model ptype =
    div
        [ class "section"
        ]
        [ button
            [ onClick (NewProcess ptype)
            , class "button"
            ]
            [ text <| "New " ++ ptype.name
            ]
        , div [ class "columns is-multiline" ]
            (DictSet.filter (\p -> p.type_ == ptype.name) model.processes
                |> DictSet.toList
                |> List.sortBy P.compare
                |> List.reverse
                |> List.map viewThumbnail
            )
        ]


viewThumbnail : Process -> Html Msg
viewThumbnail p =
    div [ class "column is-one-quarter" ]
        [ a [ href <| "/process/" ++ Uuid.toString p.uuid ]
            [ div [ class "box", id <| Uuid.toString p.uuid ]
                [ text "process"
                , br [] []
                , text <| Uuid.toString p.uuid
                ]
            ]
        ]
