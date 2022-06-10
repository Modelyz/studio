module Page.ProcessType exposing (match, page, view)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Input as Input
import Event
import Html.Attributes as Attr
import REA.EntityType as ENT exposing (EntityType)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View, closeMenu)
import View.Navbar as Navbar


type alias Model =
    { route : Route
    , inputProcessType : EntityType
    , ptype : EntityType
    }


type Msg
    = ProcessTypeChanged EntityType
    | InputProcessName String


type alias Flags =
    { route : Route }


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
        Route.ProcessType _ ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , inputProcessType = ENT.ProcessType { name = "", parent = Nothing }
      , ptype = ENT.ProcessType { name = "", parent = Nothing }
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputProcessName name ->
            let
                ptype =
                    model.inputProcessType
            in
            ( { model | inputProcessType = ENT.ProcessType { name = name, parent = ENT.toParent ptype } }, Effect.none )

        ProcessTypeChanged pt ->
            ( { model
                | inputProcessType = pt
              }
            , Shared.dispatch s <| Event.ProcessTypeChanged pt
            )


view : Shared.Model -> Model -> View Msg
view _ model =
    { title = "Process Type"
    , attributes = []
    , element = \_ -> viewContent model
    , route = model.route
    }


viewContent : Model -> Element Msg
viewContent model =
    let
        name =
            ENT.toName model.inputProcessType
    in
    column
        []
        [ column
            []
            [ column []
                [ paragraph []
                    [ text "Process Type"
                    ]
                , paragraph [] [ text "Configuration of the type of processes managed by this service" ]
                ]
            ]
        , column
            []
            [ column
                []
                [ text "Process name:" ]
            , column []
                [ Input.text [ View.onEnter <| ProcessTypeChanged model.inputProcessType ]
                    { onChange = InputProcessName
                    , text = name
                    , placeholder =
                        if name == "" then
                            Just <| Input.placeholder [] <| text "Enter the name of the processes to create"

                        else
                            Nothing
                    , label = Input.labelLeft [] <| text "Process name"
                    }
                ]
            , column []
                [ column
                    []
                    [ Input.button
                        [ htmlAttribute <|
                            Attr.disabled
                                (model.inputProcessType == model.ptype)
                        ]
                        { onPress = Just <| ProcessTypeChanged model.inputProcessType
                        , label = text "Change"
                        }
                    ]
                ]
            ]
        ]
