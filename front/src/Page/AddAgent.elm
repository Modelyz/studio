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
import REA.Entity as Entity
import REA.EntityType as ENT
import REA.Ident as I exposing (Identifier)
import Random.Pcg.Extended as Random exposing (Seed, initialSeed)
import Result exposing (andThen)
import Route exposing (Route)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.FlatSelect exposing (flatselect)
import View.InputIdentifiers exposing (..)
import View.Navbar as Navbar
import View.Radio as Radio
import View.Step as Step exposing (Step, isFirst, nextOrValidate, nextStep, previousStep)


type Msg
    = InputType (Maybe AgentType)
    | InputIdentifier Identifier
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
    , identifiers : DictSet String Identifier
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepType
    | StepIdentifiers


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
      , identifiers = Set.filter (\i -> i.entity == Entity.Agent) s.state.identifierTypes |> Set.map I.compareIdentifier I.fromIdentifierType
      , warning = ""
      , step = Step.Step StepType
      , steps = [ Step.Step StepType, Step.Step StepIdentifiers ]
      }
    , closeMenu s
    )


isChildOfAny : Shared.Model -> ENT.EntityTypes -> AgentType -> Bool
isChildOfAny s ets at =
    -- the agent type (of the new agent) must be a child of one of the entity types of the identifier type
    case ets of
        ENT.AgentTypes ats ->
            ats |> Set.toList |> List.any (isChild s at)

        _ ->
            False


isChild : Shared.Model -> AgentType -> String -> Bool
isChild s child item =
    -- true if child is really a child of item
    let
        mitem =
            getAgentType s item
    in
    Maybe.map
        (\at ->
            if at.name == child.name then
                True

            else
                Maybe.map (\p -> isChild s at p) child.type_ |> Maybe.withDefault False
        )
        mitem
        |> Maybe.withDefault False


getAgentType : Shared.Model -> String -> Maybe AgentType
getAgentType s name =
    Set.filter (\at -> at.name == name) s.state.agentTypes |> Set.toList |> List.head


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputType mat ->
            ( { model
                | flatselect = mat
                , identifiers =
                    Set.filter
                        (\it ->
                            (it.entity == Entity.Agent)
                                && (Maybe.map (\at -> isChildOfAny s it.applyTo at) mat |> Maybe.withDefault False)
                        )
                        s.state.identifierTypes
                        |> Set.map I.compareIdentifier I.fromIdentifierType
              }
            , Effect.none
            )

        InputIdentifier i ->
            ( { model | identifiers = Set.insert i model.identifiers }, Effect.none )

        Warning err ->
            ( { model | warning = err }, Effect.none )

        Added ->
            case validate model of
                Ok a ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatch s <| Event.AgentAdded a
                        , redirect s Route.Agents
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        PreviousPage ->
            case previousStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, redirect s Route.Agents )

        NextPage ->
            case nextStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, redirect s Route.Agents )

        Cancel ->
            ( model, redirect s Route.Agents )


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
        Step.Step StepType ->
            nextOrValidate model NextPage Added (checkNothing model.flatselect "Please choose a name")

        Step.Step StepIdentifiers ->
            nextOrValidate model NextPage Added (Ok model.identifiers)


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
                        , explain = h2 "Choose the Type of the new Agent"
                        }

                Step.Step StepIdentifiers ->
                    inputIdentifiers
                        { onEnter = Added
                        , onInput = InputIdentifier
                        }
                        Entity.Agent
                        model
    in
    cardContent s
        "Adding an Agent"
        buttons
        [ step
        ]
