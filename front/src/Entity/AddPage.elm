module Entity.AddPage exposing (..)

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
import Entity.Type as Type exposing (Type)
import Event.Event as Event exposing (Event)
import Group.Group as Group exposing (Group)
import Html.Attributes as Attr
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType as IdentifierType
import Ident.Input exposing (inputIdentifiers)
import Ident.Scope exposing (Scope(..))
import Message
import Navbar
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process as Process exposing (Process)
import Random.Pcg.Extended as Random exposing (Seed, initialSeed)
import Resource.Resource as Resource exposing (Resource)
import Result exposing (andThen)
import Route exposing (Route, redirectParent)
import Shared
import Spa.Page
import Style exposing (..)
import Time exposing (millisToPosix)
import View exposing (View, button, checkNothing, closeMenu, floatingContainer, h2)
import View.FlatSelect exposing (flatselect)
import View.Radio as Radio
import View.Step as Step exposing (Step, isFirst, nextOrValidate, nextStep, previousStep)


type Msg
    = InputType (Maybe Entity)
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
    , flatselect : Maybe Entity
    , identifiers : DictSet String Identifier
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepType
    | StepIdentifiers


type alias Config a =
    { filter : DictSet String Entity -> DictSet String Entity
    , typeExplain : String
    , pageTitle : String
    , constructor : a -> Entity
    , currentType : Type
    , validate : Model -> Result String Entity
    }


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        ( newUuid, newSeed ) =
            Random.step Uuid.generator s.currentSeed
    in
    ( { route = f.route
      , flatselect = Nothing
      , uuid = newUuid
      , identifiers =
            Debug.log "identifiers="
                (s.state.identifierTypes
                    |> Set.filter
                        (\it -> IdentifierType.within it s.state.entities Nothing)
                    |> Set.map Identifier.compare (Identifier.fromIdentifierType newUuid)
                )
      , warning = ""
      , step = Step.Step StepType
      , steps = [ Step.Step StepType, Step.Step StepIdentifiers ]
      }
    , closeMenu f s.menu
    )


update : Config a -> Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update c s msg model =
    case msg of
        InputType met ->
            ( { model
                | flatselect = met
                , identifiers =
                    -- select the identifiers corresponding to the chosen type
                    s.state.identifierTypes
                        |> Set.filter
                            (\it -> IdentifierType.within it s.state.entities met)
                        |> Set.map Identifier.compare (Identifier.fromIdentifierType model.uuid)
              }
            , Effect.none
            )

        InputIdentifier i ->
            ( { model | identifiers = Set.insert i model.identifiers }, Effect.none )

        Warning err ->
            ( { model | warning = err }, Effect.none )

        Added ->
            case c.validate model of
                Ok e ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatchMany s
                            (Message.Added e :: List.map Message.IdentifierAdded (Set.toList model.identifiers))
                        , redirectParent s.navkey model.route |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        PreviousPage ->
            case previousStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, redirectParent s.navkey model.route |> Effect.fromCmd )

        NextPage ->
            case nextStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, redirectParent s.navkey model.route |> Effect.fromCmd )

        Cancel ->
            ( model, redirectParent s.navkey model.route |> Effect.fromCmd )


view : Config a -> Shared.Model -> Model -> View Msg
view c s model =
    { title = c.pageTitle
    , attributes = []
    , element = viewContent c model
    , route = model.route
    }


buttonNext : Config a -> Model -> Element Msg
buttonNext c model =
    case model.step of
        Step.Step StepType ->
            nextOrValidate model
                NextPage
                Added
                (if (String.slice 0 4 <| String.reverse <| Type.toString c.currentType) == "epyT" then
                    Ok model.flatselect

                 else
                    Result.map Just <| checkNothing model.flatselect "Please choose a type"
                )

        Step.Step StepIdentifiers ->
            nextOrValidate model NextPage Added (Ok model.identifiers)


viewContent : Config a -> Model -> Shared.Model -> Element Msg
viewContent c model s =
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
                , buttonNext c model
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
                        { all = s.state.entities |> c.filter |> Set.toList
                        , toString = Entity.toUuidString
                        , toDesc = Entity.toUuidString >> Just
                        , onInput = InputType
                        , label = "Type"
                        , explain = h2 c.typeExplain
                        }

                Step.Step StepIdentifiers ->
                    inputIdentifiers
                        { onEnter = Added
                        , onInput = InputIdentifier
                        }
                        model
    in
    floatingContainer s
        c.pageTitle
        buttons
        [ step
        ]
