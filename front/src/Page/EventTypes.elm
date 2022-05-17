module Page.EventTypes exposing (match, page, view)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import REA.EventType as ET exposing (EventType)
import REA.ProcessType as PT exposing (ProcessType)
import REA.ProcessTypeEventType exposing (ProcessTypeEventType)
import Route exposing (Route)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.Navbar as Navbar


type alias Model =
    { route : Route
    , form : Form
    }


type Msg
    = Removed String
    | Added ( EventType, List ProcessType )
    | GotInput Form
    | Warning String
    | Link String
    | Unlink String


type alias Flags =
    { route : Route }


type alias Form =
    { name : String
    , type_ : Maybe String
    , processTypes : DictSet String String
    , warning : String
    }


empty : Form
empty =
    { name = "", type_ = Nothing, processTypes = Set.empty identity, warning = "" }


validate : Form -> Maybe ( EventType, List ProcessType )
validate f =
    -- TODO replace with Either with the error on the Left
    if f.name == "" || Set.size f.processTypes == 0 then
        Nothing

    else
        Just
            ( { name = f.name, type_ = f.type_ }
            , f.processTypes |> Set.toList |> List.map (\pt -> { name = pt })
            )


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
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route, form = empty }, closeMenu s )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        GotInput form ->
            ( { model | form = form }, Effect.none )

        Added ( etype, ptypes ) ->
            ( { model
                | form = empty
              }
            , Shared.dispatchMany s <|
                Event.EventTypeAdded { eventType = EventType etype.name etype.type_ }
                    :: List.map (\pt -> Event.LinkedEventTypeToProcessType { etype = etype.name, ptype = pt.name }) ptypes
            )

        Removed etype ->
            let
                form =
                    model.form
            in
            -- TODO UnlinkedEventTypeToProcessType ?
            ( { model | form = { form | warning = "" } }
            , Shared.dispatch s <| Event.EventTypeRemoved etype
            )

        Warning w ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | warning = w } }, Effect.none )

        Link pt ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | processTypes = Set.insert pt model.form.processTypes } }, Effect.none )

        Unlink pt ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | processTypes = Set.remove pt model.form.processTypes } }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Event Types"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        form =
            model.form
    in
    column [ width fill, alignTop, padding 20 ]
        [ column [ Border.shadow shadowStyle, padding 20, centerX, alignTop ]
            [ column
                [ spacing 20 ]
                [ h1 "Event Types"
                , if Set.size s.state.eventTypes > 0 then
                    p "Existing Event Types:"

                  else
                    p "There are no Event Types yet. Create your first one!"
                , wrappedRow
                    [ spacing 10 ]
                    (s.state.eventTypes
                        |> Set.toList
                        |> List.map (\pt -> viewSmallCard (Removed pt.name) pt.name "")
                    )
                , column
                    [ spacing 20 ]
                    [ text "Add a new Event Type:"
                    , row []
                        [ Input.text
                            [ Input.focusedOnLoad
                            , Maybe.map Added (validate model.form)
                                |> Maybe.withDefault (Warning "Incomplete form")
                                |> View.onEnter
                            ]
                            { onChange = \n -> GotInput { form | name = n }
                            , text = model.form.name
                            , placeholder =
                                Just <| Input.placeholder [] <| text "Name of the new Event Type"
                            , label = Input.labelLeft [] <| text "Name"
                            }
                        ]
                    , column
                        [ spacing 10 ]
                        [ p "This event type is usable from the following process types:"
                        , column [ spacing 10 ]
                            (s.state.processTypes
                                |> Set.toList
                                |> List.sortBy PT.compare
                                |> List.map
                                    (\pt ->
                                        row []
                                            [ Input.checkbox
                                                []
                                                { onChange =
                                                    \b ->
                                                        if b then
                                                            Link pt.name

                                                        else
                                                            Unlink pt.name
                                                , icon = Input.defaultCheckbox
                                                , checked = Set.member pt.name model.form.processTypes
                                                , label = Input.labelRight [] <| text pt.name
                                                }
                                            ]
                                    )
                            )
                        ]
                    , row [ spacing 20 ]
                        [ button.primary
                            (Maybe.map Added (validate model.form)
                                |> Maybe.withDefault (Warning "Incomplete form")
                            )
                            "Add"
                        , if model.form.warning /= "" then
                            paragraph [ Font.color color.text.warning ] [ text model.form.warning ]

                          else
                            none
                        ]
                    ]
                ]
            ]
        ]
