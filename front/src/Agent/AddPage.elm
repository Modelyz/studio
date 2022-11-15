module Agent.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Agent.Agent exposing (Agent)
import AgentType.AgentType exposing (AgentType)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable
import Group.Input exposing (inputGroups)
import Hierarchy.Hierarchic as H
import Ident.Identifiable exposing (hWithIdentifiers)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType exposing (initIdentifiers)
import Ident.Input exposing (inputIdentifiers)
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended as Random exposing (Seed)
import Route exposing (Route, redirect)
import Scope.Scope exposing (Scope(..))
import Shared
import Spa.Page
import Type
import Typed.Type as TType
import Typed.Typed as T
import Value.Input exposing (inputValues)
import Value.Value as Value exposing (Value)
import Value.ValueType exposing (initValues)
import View exposing (..)
import View.FlatSelect exposing (hFlatselect, tFlatselect)
import View.Smallcard exposing (hClickableCard, hViewHalfCard)
import View.Step as Step exposing (Step(..), buttons)
import View.Style exposing (..)


type alias TypedType =
    Agent


type alias HierarchicType =
    AgentType


constructor =
    Agent


typedConstructor : TType.Type
typedConstructor =
    TType.Agent


hereType : Type.Type
hereType =
    Type.TType TType.Agent


mkMessage : TypedType -> Message.Payload
mkMessage =
    Message.AddedAgent


allT : Shared.Model -> Dict String Agent
allT =
    .state >> .agents


allH : Shared.Model -> Dict String AgentType
allH =
    .state >> .agentTypes


type alias Flags =
    { route : Route, uuid : Maybe Uuid }


type alias Model =
    { route : Route
    , isNew : Bool
    , uuid : Uuid
    , seed : Seed
    , type_ : Maybe HierarchicType
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , oldGroups : Dict String Group
    , groups : Dict String Group
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepType
    | StepIdentifiers
    | StepValues
    | StepGroups


type Msg
    = InputType (Maybe HierarchicType)
    | InputIdentifier Identifier
    | InputValue Value
    | InputGroups (Dict String Group)
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
        Route.Entity Route.Agent Route.Add ->
            Just { route = route, uuid = Nothing }

        Route.Entity Route.Agent (Route.Edit uuid) ->
            Just { route = route, uuid = Uuid.fromString uuid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        ( newUuid, newSeed ) =
            Random.step Uuid.generator s.currentSeed

        isNew =
            f.uuid == Nothing

        adding =
            { route = f.route
            , isNew = isNew
            , type_ = Nothing
            , uuid = newUuid
            , seed = newSeed
            , identifiers = initIdentifiers (allT s) (allH s) s.state.identifierTypes hereType Nothing newUuid isNew
            , values = initValues (allT s) (allH s) s.state.valueTypes hereType Nothing newUuid isNew
            , oldGroups = Dict.empty
            , groups = Dict.empty
            , warning = ""
            , step = Step.Step StepType
            , steps = [ Step.Step StepType, Step.Step StepIdentifiers, Step.Step StepValues, Step.Step StepGroups ]
            }
    in
    ( f.uuid
        |> Maybe.andThen (T.find (allT s))
        |> Maybe.map
            (\t ->
                let
                    oldGroups =
                        s.state.grouped
                            |> Dict.filter (\_ v -> t.uuid == Groupable.uuid v.groupable)
                            |> Dict.foldl (\_ v d -> Dict.insert (Group.compare v.group) v.group d) Dict.empty

                    parent =
                        H.find (allH s) t.type_
                in
                { adding
                    | type_ = parent
                    , uuid = t.uuid
                    , identifiers =
                        initIdentifiers (allT s) (allH s) s.state.identifierTypes hereType parent t.uuid adding.isNew
                            |> Dict.union (Identifier.fromUuid t.uuid s.state.identifiers)
                    , values =
                        initValues (allT s) (allH s) s.state.valueTypes hereType parent t.uuid adding.isNew
                            |> Dict.union (Value.fromUuid t.uuid s.state.values)
                    , oldGroups = oldGroups
                    , groups = oldGroups
                }
            )
        |> Maybe.withDefault adding
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputType mh ->
            ( { model
                | type_ = mh
                , identifiers =
                    initIdentifiers (allT s) (allH s) s.state.identifierTypes hereType mh model.uuid model.isNew
                        |> Dict.union (Identifier.fromUuid model.uuid s.state.identifiers)
                , values =
                    initValues (allT s) (allH s) s.state.valueTypes hereType mh model.uuid model.isNew
                        |> Dict.union (Value.fromUuid model.uuid s.state.values)
              }
            , Effect.none
            )

        InputIdentifier i ->
            ( { model | identifiers = Dict.insert (Identifier.compare i) i model.identifiers }, Effect.none )

        InputValue v ->
            ( { model | values = Dict.insert (Value.compare v) v model.values }, Effect.none )

        InputGroups gs ->
            ( { model | groups = gs }, Effect.none )

        Button Step.Added ->
            case validate model of
                Ok t ->
                    let
                        addedGroups =
                            Dict.diff model.groups model.oldGroups

                        removedGroups =
                            Dict.diff model.oldGroups model.groups
                    in
                    ( model
                    , Effect.batch
                        [ Shared.dispatchMany s
                            (Message.AddedAgent t
                                :: List.map Message.AddedIdentifier (Dict.values model.identifiers)
                                ++ List.map Message.AddedValue (Dict.values model.values)
                                ++ List.map (\g -> Message.Grouped (Groupable.A t) g) (Dict.values addedGroups)
                                ++ List.map (\g -> Message.Ungrouped (Groupable.A t) g) (Dict.values removedGroups)
                            )
                        , redirect s.navkey (Route.Entity Route.Agent (Route.View (Uuid.toString model.uuid))) |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding an Agent Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


checkStep : Model -> Result String ()
checkStep model =
    case model.step of
        Step StepType ->
            Maybe.map (\_ -> Ok ()) model.type_ |> Maybe.withDefault (Err "You must select an Agent Type")

        Step StepIdentifiers ->
            Ok ()

        Step StepValues ->
            Ok ()

        Step StepGroups ->
            Ok ()


validate : Model -> Result String Agent
validate m =
    case m.type_ of
        Just h ->
            -- TODO check that TType thing is useful
            Ok <| constructor typedConstructor m.uuid h.uuid Dict.empty Dict.empty Dict.empty Dict.empty

        Nothing ->
            Err "You must select an Agent Type"


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepType ->
                    hFlatselect
                        { allT = allT
                        , allH = allH
                        , mstuff = model.type_
                        , onInput = InputType
                        , title = "Type:"
                        , explain = "Choose the type of the new Agent:"
                        , empty = "(There are no Agent Types yet to choose from)"
                        }
                        s

                Step.Step StepGroups ->
                    inputGroups { onInput = InputGroups } s model

                Step.Step StepIdentifiers ->
                    inputIdentifiers { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputIdentifier } model

                Step.Step StepValues ->
                    inputValues { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputValue } s model
    in
    floatingContainer s
        (Just <| Button Step.Cancel)
        "Adding an Agent"
        (List.map (Element.map Button) (buttons model (checkStep model)))
        [ step
        ]
