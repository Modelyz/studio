module Page.AddAgentType exposing (..)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import REA.AgentType as AT exposing (AgentType)
import Result exposing (andThen)
import Route exposing (Route)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.FlatSelect exposing (flatselect)
import View.Navbar as Navbar
import View.Radio as Radio
import View.Step as Step exposing (isFirst, nextOrValidate, nextStep, previousStep)


type Msg
    = InputName String
    | InputType (Maybe AgentType)
    | Warning String
    | PreviousPage
    | NextPage
    | Cancel
    | Added


type alias Flags =
    { route : Route }


type alias Model =
    { route : Route
    , name : String
    , flatselect : Maybe AgentType
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepName
    | StepType


validate : Model -> Result String AgentType
validate m =
    Result.map2
        AgentType
        (checkEmptyString m.name "The name is Empty")
        (Ok <| Maybe.map .name m.flatselect)


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
        Route.AddAgentType ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , flatselect = Nothing
      , name = ""
      , warning = ""
      , steps = [ Step.Step StepName, Step.Step StepType ]
      , step = Step.Step StepName
      }
    , closeMenu s
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputName x ->
            ( { model | name = x }, Effect.none )

        InputType x ->
            ( { model | flatselect = x }, Effect.none )

        Warning err ->
            ( { model | warning = err }, Effect.none )

        Added ->
            case validate model of
                Ok i ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatch s <| Event.AgentTypeAdded i
                        , redirect s Route.AgentTypes
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        PreviousPage ->
            case previousStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, redirect s Route.AgentTypes )

        NextPage ->
            case nextStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, redirect s Route.AgentTypes )

        Cancel ->
            ( model, redirect s Route.AgentTypes )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding and Agent Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


buttonNext : Model -> Element Msg
buttonNext model =
    case model.step of
        Step.Step StepType ->
            nextOrValidate model NextPage Added (Ok model.flatselect)

        Step.Step StepName ->
            nextOrValidate model NextPage Added (checkEmptyString model.name "Please choose a name")


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        buttons : List (Element Msg)
        buttons =
            [ wrappedRow [ width fill, spacing 20 ]
                [ (if isFirst model.step model.steps then
                    button.disabled "This is the first page"

                   else
                    button.secondary PreviousPage
                  )
                    "← Previous"
                , button.secondary Cancel "Cancel"
                , buttonNext model
                , if model.warning /= "" then
                    paragraph [ Font.color color.text.warning ] [ text model.warning ]

                  else
                    none
                ]
            ]

        step =
            case model.step of
                Step.Step StepType ->
                    flatselect model
                        { all = Set.toList <| s.state.agentTypes
                        , toString = AT.toString
                        , toDesc = AT.toDesc
                        , onInput = InputType
                        , label = "Type"
                        , explain = h2 "Choose the type of the new Agent Type (it can be hierarchical)"
                        }

                Step.Step StepName ->
                    el [ alignTop ] <|
                        Input.text
                            [ width <| minimum 200 fill
                            , Input.focusedOnLoad
                            , View.onEnter NextPage
                            ]
                            { onChange = InputName
                            , text = model.name
                            , placeholder =
                                Just <| Input.placeholder [] <| text "Name"
                            , label = Input.labelAbove [ Font.size size.text.h3, paddingXY 0 10 ] <| text "Give a name to this new Agent Type"
                            }
    in
    cardContent s
        "Adding an Agent Type"
        buttons
        [ step
        ]
