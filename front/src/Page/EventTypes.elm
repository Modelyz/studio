module Page.EventTypes exposing (match, page, view)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Input as Input
import Event
import Html.Attributes as Attr
import REA.EventType as ET exposing (EventType)
import REA.ProcessType as PT
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


type alias Model =
    { inputEventType : String
    , inputEventTypeProcessTypes : DictSet String String
    }


type Msg
    = DeleteEventType EventType
    | InputEventType String
    | NewEventType
    | LinkToProcessType String
    | UnlinkFromProcessType String


type alias Flags =
    ()


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
        Route.EventTypes ->
            Just ()

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init _ _ =
    ( { inputEventType = ""
      , inputEventTypeProcessTypes = Set.empty identity
      }
    , Effect.none
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        NewEventType ->
            ( { model
                | inputEventType = ""
                , inputEventTypeProcessTypes = Set.empty identity
              }
            , Shared.dispatchMany s <|
                Event.EventTypeAdded { eventType = ET.new model.inputEventType }
                    :: List.map (\pt -> Event.LinkedEventTypeToProcessType { etype = model.inputEventType, ptype = pt }) (Set.toList model.inputEventTypeProcessTypes)
            )

        InputEventType etype ->
            ( { model | inputEventType = etype }, Effect.none )

        DeleteEventType etype ->
            ( model
            , Shared.dispatch s <| Event.EventTypeRemoved { eventType = etype }
            )

        LinkToProcessType pt ->
            ( { model | inputEventTypeProcessTypes = Set.insert pt model.inputEventTypeProcessTypes }, Effect.none )

        UnlinkFromProcessType pt ->
            ( { model | inputEventTypeProcessTypes = Set.remove pt model.inputEventTypeProcessTypes }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Event Types"
    , attributes = []
    , element = viewContent s model
    }


viewThumbnail : EventType -> Element Msg
viewThumbnail et =
    row []
        [ row
            [ htmlAttribute <| Attr.id et.name ]
            [ Input.button
                []
                { onPress = Just <| DeleteEventType et, label = text et.name }
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
                    [ text "Event Types"
                    ]
                , paragraph [] [ text "What kind of events may have occured in the past" ]
                ]
            ]
        , row
            []
            [ row
                []
                ((if Set.size s.state.eventTypes > 0 then
                    text "Current types:"

                  else
                    column [] []
                 )
                    :: (s.state.eventTypes
                            |> Set.toList
                            |> List.map viewThumbnail
                       )
                )
            , row
                []
                [ row []
                    [ Input.text
                        [ View.onEnter NewEventType ]
                        { onChange = InputEventType
                        , text = model.inputEventType
                        , placeholder =
                            if model.inputEventType == "" then
                                Just <| Input.placeholder [] <| text "Enter the name of a new event type"

                            else
                                Nothing
                        , label = Input.labelLeft [] <| text "Event type"
                        }
                    , row
                        []
                        [ text "Add a new Event type:" ]
                    ]
                , row []
                    [ row
                        []
                        [ text "This event type is usable from the following process types:" ]
                    , row []
                        (s.state.processTypes
                            |> Set.toList
                            |> List.sortBy PT.compare
                            |> List.map
                                (\pt ->
                                    row []
                                        [ row []
                                            [ Input.checkbox
                                                []
                                                { onChange =
                                                    \b ->
                                                        if b then
                                                            LinkToProcessType pt.name

                                                        else
                                                            UnlinkFromProcessType pt.name
                                                , icon = Input.defaultCheckbox
                                                , checked = Set.member pt.name model.inputEventTypeProcessTypes
                                                , label = Input.labelRight [] <| text pt.name
                                                }
                                            ]
                                        ]
                                )
                        )
                    ]
                , row []
                    [ row
                        []
                        [ Input.button
                            []
                            { onPress = Just NewEventType, label = text "Add" }
                        ]
                    ]
                ]
            ]
        ]
