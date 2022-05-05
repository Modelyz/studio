module Page.ProcessTypes exposing (match, page, view)

import DictSet as Set
import Effect exposing (Effect)
import Event
import Html exposing (Html, button, div, form, h1, input, label, p, span, text)
import Html.Attributes exposing (class, id, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Page.Navbar as Navbar
import REA.ProcessType exposing (ProcessType)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


type alias Model =
    { route : Route
    , inputProcessType : ProcessType
    }


type Msg
    = DeleteProcessType ProcessType
    | ProcessTypeChanged ProcessType
    | InputProcessName String


type alias Flags =
    { route : Route }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.ProcessTypes ->
            Just { route = route }

        _ ->
            Nothing


init : Flags -> ( Model, Effect Shared.Msg Msg )
init flags =
    ( { route = flags.route
      , inputProcessType = ProcessType ""
      }
    , Effect.none
    )


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Process Types"
    , attributes = []
    , element =
        Html.div []
            [ Navbar.view shared model.route
            , viewContent shared model
            ]
    }


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        InputProcessName name ->
            let
                ptype =
                    model.inputProcessType
            in
            ( { model | inputProcessType = { ptype | name = name } }, Effect.none )

        DeleteProcessType ptype ->
            ( model
            , Shared.dispatch shared <| Event.ProcessTypeRemoved { ptype = ptype.name }
            )

        ProcessTypeChanged ptype ->
            ( { model
                | inputProcessType = { name = "" }
              }
            , Shared.dispatch shared <| Event.ProcessTypeChanged { ptype = ptype }
            )


viewThumbnail : ProcessType -> Html Msg
viewThumbnail pt =
    div
        [ class "container"
        , style "background" "yellow"
        ]
        [ div
            [ class "box", id pt.name ]
            [ text pt.name
            , button
                [ class "delete is-medium"
                , onClick <| DeleteProcessType pt
                ]
                []
            ]
        ]


viewContent : Shared.Model -> Model -> Html Msg
viewContent shared model =
    div
        []
        [ div
            [ class "hero is-medium"
            ]
            [ div [ class "hero-body" ]
                [ p [ class "title" ]
                    [ text "Process Types"
                    ]
                , p [ class "subtitle" ] [ text "What kind of processes may be created" ]
                ]
            ]
        , div
            [ class "columns form"
            ]
            [ div
                [ class "column is-one-third" ]
                ((if Set.size shared.processTypes > 0 then
                    h1 [] [ text "Current types:" ]

                  else
                    span [] []
                 )
                    :: (shared.processTypes
                            |> Set.toList
                            |> List.map viewThumbnail
                       )
                )
            , div
                [ class "column is-one-third" ]
                [ label
                    [ class "label" ]
                    [ text "Add a new Process type:" ]
                , div [ class "field" ]
                    [ form
                        [ class "control"
                        , onSubmit <| ProcessTypeChanged model.inputProcessType
                        ]
                        [ input
                            [ type_ "text"
                            , value model.inputProcessType.name
                            , class "input"
                            , onInput InputProcessName
                            , placeholder "Enter the name of a new process type"
                            ]
                            []
                        ]
                    ]
                , div [ class "field" ]
                    [ div
                        [ class "control" ]
                        [ button
                            [ class "button is-link"
                            , onClick <| ProcessTypeChanged model.inputProcessType
                            ]
                            [ text "Add"
                            ]
                        ]
                    ]
                ]
            ]
        ]
