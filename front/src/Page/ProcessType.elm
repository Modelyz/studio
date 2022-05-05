module Page.ProcessType exposing (match, page, view)

import Effect exposing (Effect)
import Event
import Html exposing (Html, button, div, form, input, label, p, text)
import Html.Attributes exposing (class, disabled, placeholder, type_, value)
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
    , ptype : ProcessType
    }


type Msg
    = ProcessTypeChanged ProcessType
    | InputProcessName String


type alias Flags =
    { route : Route
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init
        , update = update s
        , view = view s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.ProcessType _ ->
            Just { route = route }

        _ ->
            Nothing


init : Flags -> ( Model, Effect Shared.Msg Msg )
init flags =
    ( { route = flags.route
      , inputProcessType = ProcessType ""
      , ptype = ProcessType ""
      }
    , Effect.none
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputProcessName name ->
            let
                ptype =
                    model.inputProcessType
            in
            ( { model | inputProcessType = { ptype | name = name } }, Effect.none )

        ProcessTypeChanged ptype ->
            ( { model
                | inputProcessType = { name = "" }
              }
            , Shared.dispatch s <| Event.ProcessTypeChanged { ptype = ptype }
            )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Process Type"
    , attributes = []
    , element =
        Html.div []
            [ Navbar.view s model.route
            , viewContent model
            ]
    }


viewContent : Model -> Html Msg
viewContent model =
    div
        []
        [ div
            [ class "hero is-medium"
            ]
            [ div [ class "hero-body" ]
                [ p [ class "title" ]
                    [ text "Process Type"
                    ]
                , p [ class "subtitle" ] [ text "Configuration of the type of processes managed by this service" ]
                ]
            ]
        , div
            [ class "columns form"
            ]
            [ div
                [ class "column is-one-third" ]
                [ label
                    [ class "label" ]
                    [ text "Process name:" ]
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
                            , placeholder "Enter the name of the processes to create"
                            ]
                            []
                        ]
                    ]
                , div [ class "field" ]
                    [ div
                        [ class "control" ]
                        [ button
                            [ class "button is-link"
                            , disabled
                                (model.inputProcessType == model.ptype)
                            , onClick <| ProcessTypeChanged model.inputProcessType
                            ]
                            [ text "Change"
                            ]
                        ]
                    ]
                ]
            ]
        ]
