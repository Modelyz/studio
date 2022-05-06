module Page.ProcessTypes exposing (match, page, view)

import DictSet as Set
import Effect exposing (Effect)
import Element exposing (..)
import Element.Input as Input
import Event
import Html.Attributes as Attr
import Page.Navbar as Navbar
import REA.ProcessType exposing (ProcessType)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


type alias Model =
    { inputProcessType : ProcessType
    }


type Msg
    = DeleteProcessType ProcessType
    | ProcessTypeChanged ProcessType
    | InputProcessName String


type alias Flags =
    ()


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
        Route.ProcessTypes ->
            Just ()

        _ ->
            Nothing


init : Flags -> ( Model, Effect Shared.Msg Msg )
init flags =
    ( { inputProcessType = ProcessType ""
      }
    , Effect.none
    )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Process Types"
    , attributes = []
    , element = viewContent s model
    }


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputProcessName name ->
            let
                ptype =
                    model.inputProcessType
            in
            ( { model | inputProcessType = { ptype | name = name } }, Effect.none )

        DeleteProcessType ptype ->
            ( model
            , Shared.dispatch s <| Event.ProcessTypeRemoved { ptype = ptype.name }
            )

        ProcessTypeChanged ptype ->
            ( { model
                | inputProcessType = { name = "" }
              }
            , Shared.dispatch s <| Event.ProcessTypeChanged { ptype = ptype }
            )


viewThumbnail : ProcessType -> Element Msg
viewThumbnail pt =
    row
        []
        [ row
            [ htmlAttribute <| Attr.id pt.name ]
            [ Input.button []
                { onPress = Just <| DeleteProcessType pt, label = text pt.name }
            ]
        ]


viewContent : Shared.Model -> Model -> Element Msg
viewContent s model =
    row
        []
        [ row
            []
            [ row []
                [ paragraph []
                    [ text "Process Types"
                    ]
                , paragraph [] [ text "What kind of processes may be created" ]
                ]
            ]
        , row
            []
            [ row
                []
                ((if Set.size s.state.processTypes > 0 then
                    text "Current types:"

                  else
                    column [] []
                 )
                    :: (s.state.processTypes
                            |> Set.toList
                            |> List.map viewThumbnail
                       )
                )
            , row
                []
                [ text "Add a new Process type:"
                , row []
                    [ Input.text
                        [ View.onEnter <| ProcessTypeChanged model.inputProcessType ]
                        { onChange = InputProcessName
                        , text = model.inputProcessType.name
                        , placeholder =
                            if model.inputProcessType.name == "" then
                                Just <| Input.placeholder [] <| text "Enter the name of a new process type"

                            else
                                Nothing
                        , label = Input.labelLeft [] <| text "Process type"
                        }
                    ]
                ]
            , row []
                [ row
                    []
                    [ Input.button []
                        { onPress = Just <| ProcessTypeChanged model.inputProcessType
                        , label = text "Add"
                        }
                    ]
                ]
            ]
        ]
