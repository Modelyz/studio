module Agent.AddPage exposing (..)

import Agent.Agent as Agent exposing (Agent)
import AgentType.AgentType as AgentType exposing (AgentType)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable exposing (Groupable)
import Group.Input exposing (inputGroups)
import Hierarchy.View exposing (toDesc)
import Ident.Identifiable exposing (hWithIdentifiers)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType exposing (initIdentifiers)
import Ident.Input exposing (inputIdentifiers, inputIdentifiers2)
import Item.Item as Item
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended as Random exposing (Seed, initialSeed)
import Route exposing (Route, redirectParent)
import Scope.Scope as Scope exposing (Scope(..))
import Shared
import Spa.Page
import Type
import Typed.Type as TType
import View exposing (..)
import View.Smallcard exposing (hClickableCard, hViewHalfCard)
import View.Step as Step exposing (Step(..), buttons, isLast)
import View.Style exposing (..)


type alias Flags =
    { route : Route }


type alias Model =
    { route : Route
    , uuid : Uuid
    , seed : Seed
    , flatselect : Maybe AgentType
    , identifiers : Dict String Identifier
    , groups : Dict String Group
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepType
    | StepIdentifiers
    | StepGroups


type Msg
    = InputType (Maybe AgentType)
    | InputIdentifier Identifier
    | InputGroups (Dict String Group)
    | Added
    | Button Step.Msg


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
        Route.AgentAdd ->
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
      , seed = newSeed
      , identifiers = initIdentifiers s.state.agents s.state.agentTypes s.state.identifierTypes (Type.TType TType.Agent) Nothing newUuid
      , groups = Dict.empty
      , warning = ""
      , step = Step.Step StepType
      , steps = [ Step.Step StepType, Step.Step StepIdentifiers, Step.Step StepGroups ]
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    let
        ( newUuid, newSeed ) =
            Random.step Uuid.generator model.seed
    in
    case msg of
        InputType mat ->
            ( { model
                | flatselect = mat
                , identifiers = initIdentifiers s.state.agents s.state.agentTypes s.state.identifierTypes (Type.TType TType.Agent) mat newUuid
                , uuid = newUuid
                , seed = newSeed
              }
            , Effect.none
            )

        InputIdentifier i ->
            ( { model | identifiers = Dict.insert (Identifier.compare i) i model.identifiers }, Effect.none )

        InputGroups gs ->
            ( { model | groups = gs }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))

        Added ->
            case validate model of
                Ok r ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatchMany s
                            (Message.AddedAgent r
                                :: List.map Message.IdentifierAdded (Dict.values model.identifiers)
                                ++ List.map (\g -> Message.Grouped (Groupable.R r) g) (Dict.values model.groups)
                            )
                        , redirectParent s.navkey model.route |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding an Agent"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


checkStep : Model -> Result String ()
checkStep model =
    case model.step of
        Step StepType ->
            checkNothing model.flatselect "Please choose a type"
                |> Result.map (\_ -> ())

        Step StepIdentifiers ->
            Ok ()

        Step StepGroups ->
            Ok ()


validate : Model -> Result String Agent
validate m =
    case m.flatselect of
        Just rt ->
            -- TODO check that TType thing is useful
            Ok <| Agent (Type.TType TType.Agent) m.uuid rt.uuid Dict.empty

        Nothing ->
            Err "You must select an Agent Type"


buttonValidate : Model -> Result String field -> Element Msg
buttonValidate m result =
    case result of
        Ok _ ->
            if isLast m.step m.steps then
                button.primary Added "Validate and finish"

            else
                none

        Err err ->
            none


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepType ->
                    let
                        allHwithIdentifiers =
                            hWithIdentifiers s.state.identifiers s.state.agentTypes
                    in
                    column [ alignTop, spacing 10, width <| minimum 200 fill ]
                        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
                            [ h2 "Type"
                            , Maybe.map (hViewHalfCard (InputType Nothing) s.state.agents allHwithIdentifiers s.state.configs) model.flatselect
                                |> Maybe.withDefault (el [ padding 5, Font.color color.text.disabled ] (text "Empty"))
                            ]
                        , h2 "Choose the type of the new Agent"
                        , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ] <|
                            (allHwithIdentifiers
                                |> Dict.values
                                |> List.map (hClickableCard InputType s.state.agents allHwithIdentifiers s.state.configs)
                                |> withDefaultContent (p "(There are no Agent Types yet)")
                            )
                        ]

                Step.Step StepGroups ->
                    inputGroups { onInput = InputGroups } s model

                Step.Step StepIdentifiers ->
                    let
                        scope =
                            model.flatselect |> Maybe.map (\h -> HasUserType (Type.TType TType.Agent) h.uuid) |> Maybe.withDefault (HasType (Type.TType TType.Agent))
                    in
                    inputIdentifiers2 { onEnter = Added, onInput = InputIdentifier } model scope
    in
    floatingContainer s
        "Adding an Agent"
        (List.map (Element.map Button) (buttons model (checkStep model))
            ++ [ buttonValidate model (checkStep model) ]
        )
        [ step
        ]
