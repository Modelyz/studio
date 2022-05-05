module Page.CommitmentTypes exposing (match, page, view)

import DictSet as Set
import Effect exposing (Effect)
import Event
import Html exposing (Html, button, div, form, h1, input, label, p, span, text)
import Html.Attributes exposing (class, id, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Page.Navbar as Navbar
import REA.CommitmentType as CT exposing (CommitmentType)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


type alias Model =
    { route : Route
    , inputCommitmentType : String
    , inputCommitmentTypeProcessTypes : Set.DictSet String String
    }


type alias Flags =
    { route : Route }


type Msg
    = InputCommitmentType String
    | InputCommitmentTypeProcessType String
    | NewCommitmentType String
    | DeleteCommitmentType CommitmentType


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init s
        , update = update s
        , view = view s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.CommitmentTypes ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init _ flags =
    ( { route = flags.route
      , inputCommitmentType = ""
      , inputCommitmentTypeProcessTypes = Set.empty identity
      }
    , Effect.none
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputCommitmentType ctype ->
            ( { model | inputCommitmentType = ctype }, Effect.none )

        DeleteCommitmentType ctype ->
            ( model
            , Shared.dispatch s <| Event.CommitmentTypeRemoved { commitmentType = ctype }
            )

        InputCommitmentTypeProcessType pt ->
            ( { model | inputCommitmentTypeProcessTypes = Set.insert pt model.inputCommitmentTypeProcessTypes }, Effect.none )

        NewCommitmentType name ->
            ( { model
                | inputCommitmentType = ""
                , inputCommitmentTypeProcessTypes = Set.empty identity
              }
            , Shared.dispatch s <| Event.CommitmentTypeAdded { commitmentType = CT.new name }
            )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Commitment Types"
    , attributes = []
    , element =
        Html.div []
            [ Navbar.view s model.route
            , viewContent s model
            ]
    }


viewThumbnail : CommitmentType -> Html Msg
viewThumbnail ct =
    div
        [ class "container"
        , style "background" "yellow"
        ]
        [ div
            [ class "box", id ct.name ]
            [ text ct.name
            , button
                [ class "delete is-medium"
                , onClick <| DeleteCommitmentType ct
                ]
                []
            ]
        ]


viewContent : Shared.Model -> Model -> Html Msg
viewContent s model =
    div
        []
        [ div
            [ class "hero is-medium"
            ]
            [ div [ class "hero-body" ]
                [ p [ class "title" ]
                    [ text "Commitment Types"
                    ]
                , p [ class "subtitle" ] [ text "What kind of events may be expected to happen in the future" ]
                ]
            ]
        , div
            [ class "columns form"
            ]
            [ div
                [ class "column is-one-third" ]
                ((if Set.size s.state.commitmentTypes > 0 then
                    h1 [] [ text "Current types:" ]

                  else
                    span [] []
                 )
                    :: (s.state.commitmentTypes
                            |> Set.toList
                            |> List.map viewThumbnail
                       )
                )
            , div
                [ class "column is-one-third" ]
                [ label
                    [ class "label" ]
                    [ text "Add a new Commitment type:" ]
                , div [ class "field" ]
                    [ form
                        [ class "control"
                        , onSubmit <| NewCommitmentType model.inputCommitmentType
                        ]
                        [ input
                            [ type_ "text"
                            , value model.inputCommitmentType
                            , class "input"
                            , onInput InputCommitmentType
                            , placeholder "Enter the name of a new commitment type"
                            ]
                            []
                        ]
                    ]
                , div [ class "field" ]
                    [ div
                        [ class "control" ]
                        [ button
                            [ class "button is-link"
                            , onClick <| NewCommitmentType model.inputCommitmentType
                            ]
                            [ text "Add"
                            ]
                        ]
                    ]
                ]
            ]
        ]
