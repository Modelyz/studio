module Agent.AddPage exposing (..)

import Agent.Agent as Agent exposing (Agent)
import Commitment.Commitment as Commitment exposing (Commitment)
import Contract.Contract as Contract exposing (Contract)
import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Entity.Entity as Entity exposing (Entity)
import EntityType.EntityType as EntityType exposing (EntityType)
import Event.Event as Event exposing (Event)
import Group.Group as Group exposing (Group)
import Html.Attributes as Attr
import Ident.Identifiable as Identifiable
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.Input exposing (inputIdentifiers)
import Ident.Scope exposing (Scope(..))
import Message
import Navbar
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process as Process exposing (Process)
import Random.Pcg.Extended as Random exposing (Seed, initialSeed)
import Resource.Resource as Resource exposing (Resource)
import Result exposing (andThen)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import Style exposing (..)
import Time exposing (millisToPosix)
import View exposing (..)
import View.FlatSelect exposing (flatselect)
import View.Radio as Radio
import View.Step as Step exposing (Step, isFirst, nextOrValidate, nextStep, previousStep)


type Msg
    = InputType (Maybe EntityType)
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
    , uuid : Uuid
    , flatselect : Maybe EntityType
    , identifiers : DictSet String Identifier
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepType
    | StepIdentifiers


validate : Model -> Result String Entity
validate m =
    case m.flatselect of
        Just (EntityType.ResourceType t) ->
            Ok (Entity.Resource (Resource m.uuid t.name))

        Just (EntityType.EventType t) ->
            Ok (Entity.Event (Event m.uuid t.name (millisToPosix 0)))

        Just (EntityType.AgentType t) ->
            Ok (Entity.Agent (Agent m.uuid t.name))

        Just (EntityType.CommitmentType t) ->
            Ok (Entity.Commitment (Commitment m.uuid t.name (millisToPosix 0)))

        Just (EntityType.ContractType t) ->
            Ok (Entity.Contract (Contract m.uuid t.name))

        Just (EntityType.ProcessType t) ->
            Ok (Entity.Process (Process m.uuid t.name (millisToPosix 0)))

        Just (EntityType.GroupType t) ->
            Ok (Entity.Group (Group m.uuid t.name))

        Nothing ->
            Err "You must select an Entity Type"


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
      , uuid = newUuid
      , identifiers = Set.empty Identifier.compare
      , warning = ""
      , step = Step.Step StepType
      , steps = [ Step.Step StepType, Step.Step StepIdentifiers ]
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputType met ->
            case met of
                Nothing ->
                    ( { model
                        | flatselect = Nothing
                        , identifiers = Set.empty Identifier.compare
                      }
                    , Effect.none
                    )

                Just et ->
                    ( { model
                        | flatselect = Just et
                        , identifiers =
                            s.state.identifierTypes
                                |> Set.filter
                                    (\it ->
                                        Shared.isChildOfAny s
                                            (it.applyTo
                                                |> Set.toList
                                                |> List.concatMap
                                                    (\i ->
                                                        case i of
                                                            AllEntities e ->
                                                                [ e ]

                                                            AllEntityTypes e ->
                                                                [ e ]

                                                            _ ->
                                                                []
                                                    )
                                                |> Set.fromList EntityType.compare
                                            )
                                            et
                                    )
                                |> Set.map Identifier.compare Identifier.fromIdentifierType
                      }
                    , Effect.none
                    )

        InputIdentifier i ->
            ( { model | identifiers = Set.insert i model.identifiers }, Effect.none )

        Warning err ->
            ( { model | warning = err }, Effect.none )

        Added ->
            case validate model of
                Ok e ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatchMany s
                            (Message.Added e
                                :: List.map (\i -> Message.IdentifierAdded { identifiable = Identifiable.Entity e, identifier = i }) (Set.toList model.identifiers)
                            )
                        , redirect s.navkey Route.Agents |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        PreviousPage ->
            case previousStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, redirect s.navkey Route.Agents |> Effect.fromCmd )

        NextPage ->
            case nextStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, redirect s.navkey Route.Agents |> Effect.fromCmd )

        Cancel ->
            ( model, redirect s.navkey Route.Agents |> Effect.fromCmd )


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
            nextOrValidate model NextPage Added (checkNothing model.flatselect "Please choose a type")

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
                        { all = s.state.entityTypes |> EntityType.only "AgentType" |> Set.toList
                        , toString = EntityType.toName
                        , toDesc = EntityType.toParent
                        , onInput = InputType
                        , label = "Type"
                        , explain = h2 "Choose the Type of the new Agent"
                        }

                Step.Step StepIdentifiers ->
                    inputIdentifiers
                        { onEnter = Added
                        , onInput = InputIdentifier
                        }
                        model
    in
    cardContent s
        "Adding an Agent"
        buttons
        [ step
        ]
