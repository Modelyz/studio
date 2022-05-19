module Page.AddAgent exposing (..)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import Prng.Uuid as Uuid exposing (Uuid)
import REA.Agent as A exposing (Agent)
import REA.AgentType as AT exposing (AgentType)
import Random.Pcg.Extended as Random exposing (Seed, initialSeed)
import Result exposing (andThen)
import Route exposing (Route)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.FlatSelect exposing (flatselect)
import View.Navbar as Navbar
import View.Radio as Radio


type Msg
    = InputType (Maybe AgentType)
    | Warning String
    | PreviousPage
    | NextPage
    | Cancel
    | Added


type alias Flags =
    { route : Route }


type alias Model =
    { route : Route
    , name : Uuid
    , flatselect : Maybe AgentType
    , warning : String
    , step : View.Step Step
    , steps : List (View.Step Step)
    }


type Step
    = StepType



--| StepIdentifiers


validate : Model -> Result String Agent
validate m =
    Result.map2
        Agent
        (Ok m.name)
        (checkNothing m.flatselect "You must select an Agent Type" |> Result.map .name)


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
        Route.AddAgent ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        ( newUuid, newSeed ) =
            Random.step Uuid.generator s.currentSeed
    in
    ( { route = f.route
      , flatselect = Nothing
      , name = newUuid
      , warning = ""
      , step = View.Step StepType
      , steps = [ View.Step StepType ]
      }
    , closeMenu s
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputType x ->
            ( { model | flatselect = x }, Effect.none )

        Warning err ->
            ( { model | warning = err }, Effect.none )

        Added ->
            case validate model of
                Ok a ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatch s <| Event.AgentAdded { name = a.name, type_ = a.type_ }
                        , goTo s Route.Agents
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        PreviousPage ->
            case previousStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, goTo s Route.Agents )

        NextPage ->
            case nextStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, goTo s Route.Agents )

        Cancel ->
            ( model, goTo s Route.Agents )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding an Agent"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


buttonNext : Model -> Element Msg
buttonNext model =
    case model.step of
        View.Step StepType ->
            nextOrValidate model NextPage Added (checkNothing model.flatselect "Please choose a name")



--        View.Step StepIdentifiers ->
--            nextOrValidate model NextPage Added (checkEmptyString model.name "Please choose a name")


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
                View.Step StepType ->
                    flatselect model
                        { all = Set.toList <| s.state.agentTypes
                        , toString = AT.toString
                        , toDesc = AT.toDesc
                        , inputmsg = InputType
                        , label = "Type"
                        , explain = h2 "Choose the Type of the new Agent"
                        }
    in
    cardContent s
        "Adding an Agent"
        buttons
        [ step
        ]
