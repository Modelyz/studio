module Commitment.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Agent.Agent exposing (Agent)
import Commitment.Commitment exposing (Commitment)
import CommitmentType.CommitmentType exposing (CommitmentType)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Expression exposing (Expression)
import Expression.Input
import Flow exposing (Flow)
import Flow.Input
import Group.Group as Group exposing (Group)
import Group.Input exposing (inputGroups)
import Group.Link exposing (Link)
import Hierarchy.Type as HType
import Ident.Identifiable exposing (getIdentifiers)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.Input exposing (inputIdentifiers)
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended as Random exposing (Seed)
import Route exposing (Route, redirect)
import Scope.Scope as Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Shared
import Spa.Page
import Time exposing (millisToPosix)
import Type
import Typed.Type as TType
import Util exposing (checkMaybe, chooseIfSingleton, third)
import Value.Input exposing (inputValues)
import Value.Valuable exposing (getValues)
import Value.Value as Value exposing (Value)
import View exposing (..)
import View.FlatSelect exposing (flatSelect)
import View.Step as Step exposing (Step(..), buttons)


constructor =
    Commitment


typedConstructor : TType.Type
typedConstructor =
    TType.Commitment


hereType : Type.Type
hereType =
    Type.TType TType.Commitment


type alias Flags =
    { route : Route, uuid : Maybe Uuid, tuuid : Maybe String }


type alias Model =
    { route : Route
    , isNew : Bool
    , uuid : Uuid
    , seed : Seed
    , type_ : Maybe Uuid
    , commitmentType : Maybe CommitmentType
    , provider : Maybe Uuid
    , receiver : Maybe Uuid
    , qty : Maybe Expression
    , flow : Maybe Flow
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , oldGroups : Dict String Uuid
    , groups : Dict String Uuid
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepType
    | StepProvider
    | StepReceiver
    | StepFlow
    | StepIdentifiers
    | StepValues
    | StepGroups


type Msg
    = SelectType (Maybe Uuid)
    | SelectProvider (Maybe Uuid)
    | SelectReceiver (Maybe Uuid)
    | InputQty Expression
    | InputFlow (Maybe Flow)
    | InputIdentifier Identifier
    | InputValue Value
    | InputGroups (Dict String Uuid)
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
        Route.Entity Route.Commitment (Route.Add tuuid) ->
            Just { route = route, uuid = Nothing, tuuid = tuuid }

        Route.Entity Route.Commitment (Route.Edit uuid tuuid) ->
            Just { route = route, uuid = Uuid.fromString uuid, tuuid = tuuid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        ( newUuid, newSeed ) =
            Random.step Uuid.generator s.currentSeed

        isNew =
            f.uuid == Nothing

        type_ =
            Maybe.andThen Uuid.fromString f.tuuid

        adding =
            { route = f.route
            , isNew = isNew
            , type_ = type_
            , commitmentType = Nothing
            , provider = Nothing
            , receiver = Nothing
            , qty = Nothing
            , flow = Nothing
            , uuid = newUuid
            , seed = newSeed
            , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType newUuid type_ True
            , values = getValues s.state.types s.state.valueTypes s.state.values hereType newUuid type_ True
            , oldGroups = Dict.empty
            , groups = Dict.empty
            , warning = ""
            , step = Step.Step StepType
            , steps = [ Step.Step StepType, Step.Step StepProvider, Step.Step StepReceiver, Step.Step StepFlow, Step.Step StepIdentifiers, Step.Step StepValues, Step.Step StepGroups ]
            }
    in
    ( f.uuid
        |> Maybe.map
            (\uuid ->
                let
                    oldGroups =
                        s.state.grouped
                            |> Dict.filter (\_ link -> uuid == link.groupable)
                            |> Dict.values
                            |> List.map (\link -> ( Uuid.toString link.group, link.group ))
                            |> Dict.fromList

                    t =
                        Dict.get (Uuid.toString uuid) s.state.types |> Maybe.andThen third

                    commitment =
                        Dict.get (Uuid.toString uuid) s.state.commitments
                in
                { adding
                    | type_ = commitment |> Maybe.map .type_
                    , uuid = uuid
                    , commitmentType = type_ |> Maybe.andThen (\puuid -> Dict.get (Uuid.toString puuid) s.state.commitmentTypes)
                    , provider = commitment |> Maybe.map .provider
                    , receiver = commitment |> Maybe.map .receiver
                    , qty = commitment |> Maybe.map .qty
                    , flow = commitment |> Maybe.map .flow
                    , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType uuid t False
                    , values = getValues s.state.types s.state.valueTypes s.state.values hereType uuid t False
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
        SelectType mh ->
            let
                mct =
                    mh |> Maybe.andThen (\uid -> Dict.get (Uuid.toString uid) s.state.commitmentTypes)
            in
            ( { model
                | type_ = mh
                , commitmentType = mct
                , provider =
                    chooseIfSingleton
                        (s.state.agents
                            |> Dict.filter (\_ a -> mct |> Maybe.map (\ct -> containsScope s.state.types (IsItem (Type.TType a.what) a.uuid) ct.providers) |> Maybe.withDefault True)
                            |> Dict.map (\_ a -> a.uuid)
                            |> Dict.values
                        )
                , receiver =
                    chooseIfSingleton
                        (s.state.agents
                            |> Dict.filter (\_ a -> mct |> Maybe.map (\ct -> containsScope s.state.types (IsItem (Type.TType a.what) a.uuid) ct.receivers) |> Maybe.withDefault True)
                            |> Dict.map (\_ a -> a.uuid)
                            |> Dict.values
                        )
                , qty = Maybe.map .qty mct
                , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType model.uuid mh True
                , values = getValues s.state.types s.state.valueTypes s.state.values hereType model.uuid mh True
              }
            , Effect.none
            )

        SelectProvider uuid ->
            ( { model | provider = uuid }, Effect.none )

        SelectReceiver uuid ->
            ( { model | receiver = uuid }, Effect.none )

        InputQty qty ->
            ( { model | qty = Just qty }, Effect.none )

        InputFlow flow ->
            ( { model | flow = flow }, Effect.none )

        InputIdentifier i ->
            ( { model | identifiers = Dict.insert (Identifier.compare i) i model.identifiers }, Effect.none )

        InputValue v ->
            ( { model | values = Dict.insert (Value.compare v) v model.values }, Effect.none )

        InputGroups uuids ->
            ( { model | groups = uuids }, Effect.none )

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
                            (Message.AddedCommitment t
                                :: List.map Message.AddedIdentifier (Dict.values model.identifiers)
                                ++ List.map Message.AddedValue (Dict.values model.values)
                                ++ List.map (\uuid -> Message.Grouped (Link hereType t.uuid uuid)) (Dict.values addedGroups)
                                ++ List.map (\uuid -> Message.Ungrouped (Link hereType t.uuid uuid)) (Dict.values removedGroups)
                            )
                        , redirect s.navkey (Route.Entity Route.Commitment (Route.View (Uuid.toString model.uuid) (Maybe.map Uuid.toString model.type_))) |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding an Commitment"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


checkStep : Model -> Result String ()
checkStep model =
    case model.step of
        Step StepType ->
            checkMaybe model.type_ "You must select a Commitment Type" |> Result.map (\_ -> ())

        Step StepProvider ->
            checkMaybe model.provider "You must select a Provider" |> Result.map (\_ -> ())

        Step StepReceiver ->
            checkMaybe model.receiver "You must select a Receiver" |> Result.map (\_ -> ())

        Step StepFlow ->
            checkMaybe model.flow "You must input a Resource or Resource Type Flow" |> Result.map (\_ -> ())

        Step StepIdentifiers ->
            Ok ()

        Step StepValues ->
            Ok ()

        Step StepGroups ->
            Ok ()


validate : Model -> Result String Commitment
validate m =
    Result.map5
        (\type_ provider receiver flow qty -> constructor typedConstructor m.uuid type_ (millisToPosix 0) provider receiver flow qty)
        (checkMaybe m.type_ "You must select a Commitment Type")
        (checkMaybe m.provider "You must select a Provider")
        (checkMaybe m.receiver "You must select a Receiver")
        (checkMaybe m.flow "You must input a Resource or Resource Type Flow")
        (checkMaybe m.qty "The quantity is invalid")


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepType ->
                    flatSelect s
                        { what = Type.HType HType.CommitmentType
                        , muuid = model.type_
                        , onInput = SelectType
                        , title = "Type:"
                        , explain = "Choose the type of the new Commitment:"
                        , empty = "(There are no Commitment Types yet to choose from)"
                        }
                        (s.state.commitmentTypes |> Dict.map (\_ a -> a.uuid))

                Step.Step StepProvider ->
                    Maybe.map
                        (\ct ->
                            column [ spacing 20 ]
                                [ flatSelect s
                                    { what = Type.TType TType.Agent
                                    , muuid = model.provider
                                    , onInput = SelectProvider
                                    , title = "Provider:"
                                    , explain = "Choose the provider of the commitment:"
                                    , empty = "(There are no agents yet to choose from)"
                                    }
                                    (s.state.agents
                                        |> Dict.filter (\_ a -> containsScope s.state.types (IsItem (Type.TType a.what) a.uuid) ct.providers)
                                        |> Dict.map (\_ a -> a.uuid)
                                    )
                                ]
                        )
                        model.commitmentType
                        |> Maybe.withDefault none

                Step.Step StepReceiver ->
                    Maybe.map
                        (\ct ->
                            column [ spacing 20 ]
                                [ flatSelect s
                                    { what = Type.TType TType.Agent
                                    , muuid = model.receiver
                                    , onInput = SelectReceiver
                                    , title = "Receiver:"
                                    , explain = "Choose the receiver of the commitment:"
                                    , empty = "(There are no agents yet to choose from)"
                                    }
                                    (s.state.agents
                                        |> Dict.filter (\_ a -> containsScope s.state.types (IsItem (Type.TType a.what) a.uuid) ct.receivers)
                                        |> Dict.map (\_ a -> a.uuid)
                                    )
                                ]
                        )
                        model.commitmentType
                        |> Maybe.withDefault none

                Step.Step StepFlow ->
                    Maybe.map2
                        (\ct qty ->
                            column [ spacing 20 ]
                                [ h2 "Flow from the provider to the receiver"
                                , Expression.Input.inputExpression
                                    { onEnter = Step.nextMsg model Button Step.NextPage Step.Added
                                    , onInput = InputQty
                                    , context = ( hereType, model.uuid )
                                    }
                                    s
                                    ( [], qty )
                                    qty
                                , Flow.Input.input
                                    { flow = model.flow
                                    , scope = ct.flowscope
                                    , onSelect = InputFlow
                                    , onEnter = Step.nextMsg model Button Step.NextPage Step.Added
                                    }
                                    s
                                ]
                        )
                        model.commitmentType
                        model.qty
                        |> Maybe.withDefault none

                Step.Step StepGroups ->
                    inputGroups { onInput = InputGroups, type_ = hereType, mpuuid = model.type_ } s model.groups

                Step.Step StepIdentifiers ->
                    inputIdentifiers { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputIdentifier } model.identifiers

                Step.Step StepValues ->
                    inputValues
                        { onEnter = Step.nextMsg model Button Step.NextPage Step.Added
                        , onInput = InputValue
                        , context = ( Type.TType TType.Commitment, model.uuid )
                        }
                        s
                        model.values
    in
    floatingContainer s
        (Just <| Button Step.Cancel)
        "Adding an Commitment"
        (List.map (Element.map Button) (buttons model (checkStep model)))
        [ step
        ]
