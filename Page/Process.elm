module Page.Process exposing (Model, view)

import Browser exposing (Document)
import DictSet as Set
import ES exposing (getCommitments)
import Html exposing (Html, a, br, div, img, nav, span, text)
import Html.Attributes exposing (attribute, class, href, src, width)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import Page.Loading as Loading
import Page.Navbar as Navbar
import Prng.Uuid as Uuid
import REA.Commitment as C exposing (Commitment)
import REA.CommitmentType as CT exposing (CommitmentType)
import REA.Process exposing (Process)
import Status exposing (Status(..))


type alias Model =
    ES.State


view : Model -> Process -> Document Msg
view model process =
    { title = "Process"
    , body =
        [ Navbar.view model
        , Loading.wrapper model (viewContent model process)
        ]
    }


newCommitmentButton : Process -> CommitmentType -> Html Msg
newCommitmentButton process ct =
    div
        [ class "button"
        , class "hscroll"
        , onClick <| NewCommitment process ct.name
        ]
        [ text ct.name
        ]


viewContent : Model -> Process -> Html Msg
viewContent model process =
    div []
        [ div [ class "section", class "hscroll-container" ]
            [ span
                []
                [ text <| "Pizza sale # " ++ Uuid.toString process.uuid ]
            ]
        , div
            [ class "section", class "hscroll-container" ]
          <|
            List.map
                (newCommitmentButton process)
                (model.commitmentTypes |> Set.toList)
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
        [ a
            [ href <| "/commitment/" ++ Uuid.toString c.uuid ]
            [ div
                [ class "box" ]
                [ text c.name
                , br [] []
                , text <| Uuid.toString c.uuid
                ]
            ]
        ]
