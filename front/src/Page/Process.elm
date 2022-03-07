module Page.Process exposing (view)

import Browser exposing (Document)
import DictSet as Set
import Html exposing (Html, a, br, div, nav, p, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import IOStatus exposing (IOStatus(..))
import Msg exposing (Msg(..))
import Page.Navbar as Navbar
import Prng.Uuid as Uuid
import REA.Commitment as C exposing (Commitment)
import REA.CommitmentType exposing (CommitmentType)
import REA.Event as E exposing (Event)
import REA.EventType exposing (EventType)
import REA.Process exposing (Process)
import State exposing (State, getCommitmentTypes, getCommitments, getEventTypes, getEvents)


type alias Model =
    State


view : Model -> Process -> Document Msg
view model process =
    { title = "Process"
    , body =
        [ Navbar.view model
        , viewContent model process
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


newEventButton : Process -> EventType -> Html Msg
newEventButton process et =
    div
        [ class "button"
        , class "hscroll"
        , onClick <| NewEvent process et.name
        ]
        [ text et.name
        ]


viewContent : Model -> Process -> Html Msg
viewContent model process =
    let
        commitmentTypes =
            getCommitmentTypes model process.type_

        eventTypes =
            getEventTypes model process.type_
    in
    div []
        [ div
            [ class "hero is-medium"
            ]
            [ div [ class "hero-body" ]
                [ p [ class "title" ]
                    [ text <| process.type_ ++ " # " ++ Uuid.toString process.uuid
                    ]
                ]
            ]
        , div [ class "columns" ]
            [ nav [ class "panel", style "margin" "0.5rem" ]
                [ p [ class "panel-heading" ]
                    [ text "Commitments" ]
                , div [ class "panel-block hscroll-container" ] <|
                    List.map
                        (newCommitmentButton process)
                        (model.commitmentTypes
                            |> Set.filter (\ct -> Set.member ct commitmentTypes)
                            |> Set.toList
                        )
                , div [ class "panel-block" ]
                    [ div [ class "columns is-multiline" ]
                        (getCommitments model process
                            |> Set.toList
                            |> List.sortBy C.compare
                            |> List.reverse
                            |> List.map viewCommitmentThumbnail
                        )
                    ]
                ]
            , nav [ class "panel", style "margin" "0.5rem" ]
                [ p [ class "panel-heading" ]
                    [ text "Events" ]
                , div [ class "panel-block hscroll-container" ] <|
                    List.map
                        (newEventButton process)
                        (model.eventTypes
                            |> Set.filter (\et -> Set.member et eventTypes)
                            |> Set.toList
                        )
                , div [ class "panel-block" ]
                    [ div [ class "columns is-multiline" ]
                        (getEvents model process
                            |> Set.toList
                            |> List.sortBy E.compare
                            |> List.reverse
                            |> List.map viewEventThumbnail
                        )
                    ]
                ]
            ]
        ]


viewCommitmentThumbnail : Commitment -> Html Msg
viewCommitmentThumbnail c =
    div [ class "column is-one-quarter" ]
        [ a
            [ href <| "/commitment/" ++ Uuid.toString c.uuid ]
            [ div
                [ class "card" ]
                [ text c.name
                , br [] []
                , text <| Uuid.toString c.uuid
                ]
            ]
        ]


viewEventThumbnail : Event -> Html Msg
viewEventThumbnail c =
    div [ class "column is-one-quarter" ]
        [ a
            [ href <| "/event/" ++ Uuid.toString c.uuid ]
            [ div
                [ class "card" ]
                [ text c.name
                , br [] []
                , text <| Uuid.toString c.uuid
                ]
            ]
        ]
