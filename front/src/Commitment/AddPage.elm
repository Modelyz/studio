module Commitment.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Commitment.Commitment exposing (Commitment)
import CommitmentType.CommitmentType exposing (CommitmentType)
import Date
import DateTime.View
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Border as Border
import Expression exposing (Expression)
import Expression.Input
import Flow exposing (Flow(..))
import Flow.Input
import Group.Group as Group
import Group.Input exposing (inputGroups)
import Group.Link exposing (Link)
import Hierarchy.Type as HType
import Ident.Identifiable exposing (getIdentifiers)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.Input exposing (inputIdentifiers)
import Payload
import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended as Random exposing (Seed)
import Route exposing (Route, redirect)
import Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Shared
import Spa.Page
import Time exposing (millisToPosix)
import Type
import Typed.Type as TType
import Util exposing (andMapR, checkMaybe, chooseIfSingleton, third)
import Value.Input exposing (inputValues)
import Value.Valuable exposing (getValues)
import Value.Value as Value exposing (Value)
import View exposing (..)
import View.FlatSelect exposing (flatSelect)
import View.Step as Step exposing (Step(..), buttons)
import View.Style exposing (color)


typedConstructor : TType.Type
typedConstructor =
    TType.Commitment


hereType : Type.Type
hereType =
    Type.TType TType.Commitment


type alias Flags =
    { route : Route
    , uuid : Maybe Uuid
    , tuuid : Maybe String
    }


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
    , calendar : DateTime.View.Model
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , gsubmodel : Group.Input.Model
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepType
    | StepProvider
    | StepReceiver
    | StepFlow
    | StepDate
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
    | GroupMsg Group.Input.Msg
    | Button Step.Msg
    | CalendarMsg DateTime.View.Msg


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init s
        , update = update s
        , view = view
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.Entity Route.Commitment (Route.Add p) ->
            Just { route = route, uuid = Nothing, tuuid = p.type_ }

        Route.Entity Route.Commitment (Route.Edit p) ->
            Just { route = route, uuid = Uuid.fromString p.uuid, tuuid = p.type_ }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        ( newUuid, newSeed ) =
            Random.step Uuid.generator s.currentSeed

        isNew =
            f.uuid == Nothing

        wantedType =
            Maybe.andThen Uuid.fromString f.tuuid

        ( initgroups, initcmd ) =
            Group.Input.init s Dict.empty

        mct =
            wantedType |> Maybe.andThen (\uid -> Dict.get (Uuid.toString uid) s.state.commitmentTypes)

        ( calinit, calcmd ) =
            DateTime.View.init True s.zone <| Date.fromPosix s.zone <| millisToPosix 0

        adding =
            { route = f.route
            , isNew = isNew
            , type_ = wantedType
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
            , flow =
                chooseIfSingleton
                    ((s.state.resources
                        |> Dict.filter (\_ r -> mct |> Maybe.map (\ct -> containsScope s.state.types (IsItem (Type.TType r.what) r.uuid) ct.flowscope) |> Maybe.withDefault True)
                        |> Dict.map (\_ r -> ResourceFlow r)
                        |> Dict.values
                     )
                        ++ (s.state.resourceTypes
                                |> Dict.filter (\_ rt -> mct |> Maybe.map (\ct -> containsScope s.state.types (IsItem (Type.HType rt.what) rt.uuid) ct.flowscope) |> Maybe.withDefault True)
                                |> Dict.map (\_ rt -> ResourceTypeFlow rt)
                                |> Dict.values
                           )
                    )
            , qty = mct |> Maybe.map .qty
            , calendar = calinit
            , uuid = newUuid
            , seed = newSeed
            , identifiers = getIdentifiers s.state hereType newUuid wantedType True
            , values = getValues s.state.types s.state.valueTypes s.state.values hereType newUuid wantedType True
            , gsubmodel = initgroups
            , warning = ""
            , step = Step.Step StepType
            , steps = [ Step.Step StepType, Step.Step StepProvider, Step.Step StepReceiver, Step.Step StepFlow, Step.Step StepDate, Step.Step StepIdentifiers, Step.Step StepValues, Step.Step StepGroups ]
            }
    in
    f.uuid
        |> Maybe.map
            (\uuid ->
                let
                    realType =
                        Dict.get (Uuid.toString uuid) s.state.types |> Maybe.andThen third

                    gs =
                        Group.groupsOf s.state.grouped uuid |> List.map (\i -> ( Uuid.toString i, i )) |> Dict.fromList

                    ( editgroups, _ ) =
                        Group.Input.init s gs

                    commitment =
                        Dict.get (Uuid.toString uuid) s.state.commitments

                    caledit =
                        commitment |> Maybe.map .when |> Maybe.withDefault (millisToPosix 0) |> Date.fromPosix s.zone |> DateTime.View.init False s.zone
                in
                ( { adding
                    | type_ = realType
                    , uuid = uuid
                    , commitmentType = realType |> Maybe.andThen (\puuid -> Dict.get (Uuid.toString puuid) s.state.commitmentTypes)
                    , provider = commitment |> Maybe.map .provider
                    , receiver = commitment |> Maybe.map .receiver
                    , qty = commitment |> Maybe.map .qty
                    , flow = commitment |> Maybe.map .flow
                    , calendar = Tuple.first caledit
                    , identifiers = getIdentifiers s.state hereType uuid realType False
                    , values = getValues s.state.types s.state.valueTypes s.state.values hereType uuid realType False
                    , gsubmodel = editgroups
                  }
                , Effect.batch
                    [ Effect.fromCmd <| Cmd.map CalendarMsg <| Tuple.second caledit
                    , closeMenu f s.menu
                    ]
                )
            )
        |> Maybe.withDefault
            ( adding
            , Effect.batch
                [ closeMenu f s.menu
                , Effect.fromCmd <| Cmd.map CalendarMsg calcmd
                , Effect.map GroupMsg (Effect.fromCmd initcmd)
                ]
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
                , flow =
                    chooseIfSingleton
                        ((s.state.resources
                            |> Dict.filter (\_ r -> mct |> Maybe.map (\ct -> containsScope s.state.types (IsItem (Type.TType r.what) r.uuid) ct.flowscope) |> Maybe.withDefault True)
                            |> Dict.map (\_ r -> ResourceFlow r)
                            |> Dict.values
                         )
                            ++ (s.state.resourceTypes
                                    |> Dict.filter (\_ rt -> mct |> Maybe.map (\ct -> containsScope s.state.types (IsItem (Type.HType rt.what) rt.uuid) ct.flowscope) |> Maybe.withDefault True)
                                    |> Dict.map (\_ rt -> ResourceTypeFlow rt)
                                    |> Dict.values
                               )
                        )
                , qty = Maybe.map .qty mct
                , identifiers = getIdentifiers s.state hereType model.uuid mh True
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

        CalendarMsg submsg ->
            let
                mdlcmd =
                    DateTime.View.update submsg model.calendar
            in
            ( { model | calendar = Tuple.first mdlcmd }
            , Effect.fromCmd <| Cmd.map CalendarMsg <| Tuple.second mdlcmd
            )

        InputIdentifier i ->
            ( { model | identifiers = Dict.insert (Identifier.compare i) i model.identifiers }, Effect.none )

        InputValue v ->
            ( { model | values = Dict.insert (Value.compare v) v model.values }, Effect.none )

        GroupMsg submsg ->
            let
                ( submodel, subcmd ) =
                    Group.Input.update submsg model.gsubmodel
            in
            ( { model | gsubmodel = submodel }, Effect.fromCmd <| subcmd )

        Button Step.Added ->
            case validate model of
                Ok c ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatchMany s
                            (Payload.AddedCommitment c
                                :: List.map Payload.AddedIdentifier (Dict.values model.identifiers)
                                ++ List.map Payload.AddedValue (Dict.values model.values)
                                ++ List.map (\uuid -> Payload.Grouped (Link hereType c.uuid uuid)) (Dict.values <| Group.Input.added model.gsubmodel)
                                ++ List.map (\uuid -> Payload.Ungrouped (Link hereType c.uuid uuid)) (Dict.values <| Group.Input.removed model.gsubmodel)
                            )
                        , redirect s.navkey (Route.Entity Route.Commitment (Route.View { uuid = Uuid.toString model.uuid, type_ = Maybe.map Uuid.toString model.type_ })) |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))


view : Model -> View Msg
view model =
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

        Step StepDate ->
            Ok ()

        Step StepIdentifiers ->
            Ok ()

        Step StepValues ->
            Ok ()

        Step StepGroups ->
            Ok ()


validate : Model -> Result String Commitment
validate m =
    -- similar to haskell: f <$> a <*> b <*> c <*> d ...
    -- we apply multiple partial applications until we have the full value
    Result.map
        (Commitment typedConstructor m.uuid (DateTime.View.toPosix m.calendar))
        (checkMaybe m.qty "The quantity is invalid")
        |> andMapR (checkMaybe m.type_ "You must select a Commitment Type")
        |> andMapR (checkMaybe m.provider "You must select a Provider")
        |> andMapR (checkMaybe m.receiver "You must select a Receiver")
        |> andMapR (checkMaybe m.flow "You must input a Resource or Resource Type Flow")


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepType ->
                    flatSelect s.state
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
                                [ flatSelect s.state
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
                        |> Maybe.withDefault (text "This commitment has no type")

                Step.Step StepReceiver ->
                    Maybe.map
                        (\ct ->
                            column [ spacing 20 ]
                                [ flatSelect s.state
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
                        |> Maybe.withDefault (text "This commitment has no type")

                Step.Step StepFlow ->
                    Maybe.map2
                        (\ct qty ->
                            let
                                ( chosen, choice ) =
                                    Flow.Input.input
                                        { flow = model.flow
                                        , scope = ct.flowscope
                                        , onSelect = InputFlow
                                        , onEnter = Step.nextMsg model Button Step.NextPage Step.Added
                                        }
                                        s
                            in
                            column [ spacing 20 ]
                                [ h2 "Flow from the provider to the receiver"
                                , wrappedRow [ width <| minimum 50 shrink, height (px 48), padding 3, spacing 4 ]
                                    [ Expression.Input.inputExpression
                                        { onEnter = Step.nextMsg model Button Step.NextPage Step.Added
                                        , onInput = InputQty
                                        , context = ( hereType, model.uuid )
                                        }
                                        s.state
                                        ( [], qty )
                                        qty
                                    , el [ padding 3, height (px 49), Border.width 2, Border.color color.item.border ] chosen
                                    ]
                                , choice
                                ]
                        )
                        model.commitmentType
                        model.qty
                        |> Maybe.withDefault (text "This commitment has no type")

                Step.Step StepDate ->
                    column [ spacing 20, width fill ]
                        [ h1 "Select a date for the commitment"
                        , Element.map CalendarMsg <| DateTime.View.inputDate s model.calendar
                        ]

                Step.Step StepGroups ->
                    Element.map GroupMsg <| inputGroups { type_ = hereType, mpuuid = model.type_ } s.state model.gsubmodel

                Step.Step StepIdentifiers ->
                    inputIdentifiers { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputIdentifier } model.identifiers

                Step.Step StepValues ->
                    inputValues
                        { onEnter = Step.nextMsg model Button Step.NextPage Step.Added
                        , onInput = InputValue
                        , context = ( Type.TType TType.Commitment, model.uuid )
                        }
                        s.state
                        model.values
    in
    floatingContainer s
        (Just <| Button Step.Cancel)
        "Adding an Commitment"
        (List.map (Element.map Button) (buttons model (checkStep model)))
        [ step
        ]
