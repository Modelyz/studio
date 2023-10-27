module Event.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Configuration.Zone exposing (Zone(..))
import Configuration.Zone.View exposing (displayZone)
import Date
import DateTime.View
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Border as Border
import Event.Event exposing (Event)
import EventType.EventType exposing (EventType)
import Expression exposing (Expression)
import Expression.Eval as Eval
import Expression.Input
import Expression.Rational as Rational
import Expression.RationalInput as RationalInput exposing (RationalInput)
import Flow exposing (Flow(..))
import Flow.Input
import Group.Group as Group
import Group.Input exposing (inputGroups)
import Group.Link exposing (Link)
import Hierarchy.Type as HType
import Ident.Identifiable exposing (getIdentifiers)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.Input exposing (inputIdentifiers)
import Message
import Payload
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process as Process exposing (Process)
import Process.Reconcile as Reconcile exposing (Reconciliation, fromPartialProcesses, toPartialProcesses)
import Random.Pcg.Extended as Random exposing (Seed)
import Route exposing (Route, redirect)
import Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Shared
import Spa.Page
import State exposing (State)
import Time exposing (millisToPosix)
import Type
import Typed.Type as TType
import Util exposing (andMapR, checkAllOk, checkEmptyDict, checkMaybe, chooseIfSingleton, third)
import Value.Input exposing (inputValues)
import Value.Valuable exposing (getValues)
import Value.Value as Value exposing (Value)
import View exposing (..)
import View.FlatSelect exposing (flatSelect)
import View.MultiSelect exposing (multiSelect)
import View.Step as Step exposing (Step(..), buttons)
import View.Style exposing (color)


type alias Flags =
    { route : Route
    , uuid : Maybe Uuid
    , tuuid : Maybe String
    , related : Maybe Uuid
    }


type alias Model =
    { route : Route
    , isNew : Bool
    , uuid : Uuid
    , seed : Seed
    , processTypes : Dict String Uuid
    , processes : Dict String Uuid -- the list of processes this event will be part of
    , type_ : Maybe Uuid
    , eventType : Maybe EventType
    , provider : Maybe Uuid
    , receiver : Maybe Uuid
    , qty : Maybe Expression
    , flow : Maybe Flow
    , partialProcesses : List ( Uuid, RationalInput ) -- a temporary partial allocation of this event to a process
    , reconciliations : Dict String Reconciliation -- the final (with a rational) allocation of this event to the process
    , calendar : DateTime.View.Model
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , gsubmodel : Group.Input.Model
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepProcess
    | StepProcessTypes
    | StepType
    | StepProvider
    | StepReceiver
    | StepFlow
    | StepReconcile
    | StepDate
    | StepIdentifiers
    | StepValues
    | StepGroups


type Msg
    = SelectType (Maybe Uuid)
    | SelectProcessTypes (List Uuid)
    | SelectProcesses (List Uuid)
    | SelectProvider (Maybe Uuid)
    | SelectReceiver (Maybe Uuid)
    | InputQty Expression
    | InputFlow (Maybe Flow)
    | InputPartialProcesses (List ( Uuid, RationalInput ))
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
        Route.Entity Route.Event (Route.Add p) ->
            Just { route = route, uuid = Nothing, tuuid = p.type_, related = Maybe.andThen Uuid.fromString p.related }

        Route.Entity Route.Event (Route.Edit p) ->
            Just { route = route, uuid = Uuid.fromString p.uuid, tuuid = p.type_, related = Nothing }

        _ ->
            Nothing


toProcessTypes : State -> Maybe Uuid -> Dict String Uuid
toProcessTypes s muuid =
    -- return the processTypes corresponding to an eventType
    s.processTypes
        |> Dict.filter
            (\_ pt ->
                pt.eventTypes
                    |> Dict.filter (\_ et -> Just et == muuid)
                    |> Dict.isEmpty
                    |> not
            )
        |> Dict.values
        |> List.map (\pt -> ( Uuid.toString pt.uuid, pt.uuid ))
        |> Dict.fromList


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

        met =
            wantedType |> Maybe.andThen (\uid -> Dict.get (Uuid.toString uid) s.state.eventTypes)

        ( calinit, calcmd ) =
            DateTime.View.init True s.zone <| Date.fromPosix s.zone <| millisToPosix 0

        adding =
            { route = f.route
            , isNew = isNew
            , processes = f.related |> Maybe.map (\r -> Dict.singleton (Uuid.toString r) r) |> Maybe.withDefault Dict.empty
            , type_ = wantedType
            , eventType = met
            , processTypes = toProcessTypes s.state (Maybe.map .uuid met)
            , provider =
                chooseIfSingleton
                    (s.state.agents
                        |> Dict.filter (\_ a -> met |> Maybe.map (\ct -> containsScope s.state.types (IsItem (Type.TType a.what) a.uuid) ct.providers) |> Maybe.withDefault True)
                        |> Dict.map (\_ a -> a.uuid)
                        |> Dict.values
                    )
            , receiver =
                chooseIfSingleton
                    (s.state.agents
                        |> Dict.filter (\_ a -> met |> Maybe.map (\ct -> containsScope s.state.types (IsItem (Type.TType a.what) a.uuid) ct.receivers) |> Maybe.withDefault True)
                        |> Dict.map (\_ a -> a.uuid)
                        |> Dict.values
                    )
            , flow =
                chooseIfSingleton
                    ((s.state.resources
                        |> Dict.filter (\_ r -> met |> Maybe.map (\ct -> containsScope s.state.types (IsItem (Type.TType r.what) r.uuid) ct.flowscope) |> Maybe.withDefault True)
                        |> Dict.map (\_ r -> ResourceFlow r)
                        |> Dict.values
                     )
                        ++ (s.state.resourceTypes
                                |> Dict.filter (\_ rt -> met |> Maybe.map (\ct -> containsScope s.state.types (IsItem (Type.HType rt.what) rt.uuid) ct.flowscope) |> Maybe.withDefault True)
                                |> Dict.map (\_ rt -> ResourceTypeFlow rt)
                                |> Dict.values
                           )
                    )
            , partialProcesses = f.related |> Maybe.map (\r -> [ ( r, "0" ) ]) |> Maybe.withDefault []
            , reconciliations = Dict.empty
            , qty = met |> Maybe.map .qty
            , calendar = calinit
            , uuid = newUuid
            , seed = newSeed
            , identifiers = getIdentifiers s.state (Type.TType TType.Event) newUuid wantedType True
            , values = getValues s.state.types s.state.valueTypes s.state.values (Type.TType TType.Event) newUuid wantedType True
            , gsubmodel = initgroups
            , warning = ""
            , step = Step.Step StepProcessTypes
            , steps =
                [ Step.Step StepProcessTypes
                , Step.Step StepType
                , Step.Step StepProcess
                , Step.Step StepProvider
                , Step.Step StepReceiver
                , Step.Step StepFlow
                , Step.Step StepReconcile
                , Step.Step StepDate
                , Step.Step StepIdentifiers
                , Step.Step StepValues
                , Step.Step StepGroups
                ]
            }
    in
    f.uuid
        |> Maybe.map
            (\uuid ->
                let
                    realType =
                        Dict.get (Uuid.toString uuid) s.state.types |> Maybe.andThen third

                    reconciliations =
                        s.state.reconciliations |> Reconcile.filterByEvent uuid

                    gs =
                        Group.groupsOf s.state.grouped uuid |> List.map (\i -> ( Uuid.toString i, i )) |> Dict.fromList

                    ( editgroups, editcmd ) =
                        Group.Input.init s gs

                    event =
                        Dict.get (Uuid.toString uuid) s.state.events

                    caledit =
                        event |> Maybe.map .when |> Maybe.withDefault (millisToPosix 0) |> Date.fromPosix s.zone |> DateTime.View.init False s.zone

                    processes =
                        reconciliations |> Dict.values |> List.map (\r -> ( Uuid.toString r.process, r.process )) |> Dict.fromList
                in
                ( { adding
                    | processes = processes
                    , processTypes =
                        Dict.values processes
                            |> List.filterMap (\p -> Dict.get (Uuid.toString p) s.state.types |> Maybe.andThen third)
                            |> List.map (\t -> ( Uuid.toString t, t ))
                            |> Dict.fromList
                    , type_ = realType
                    , uuid = uuid
                    , eventType = realType |> Maybe.andThen (\puuid -> Dict.get (Uuid.toString puuid) s.state.eventTypes)
                    , provider = event |> Maybe.map .provider
                    , receiver = event |> Maybe.map .receiver
                    , qty = event |> Maybe.map .qty
                    , flow = event |> Maybe.map .flow
                    , partialProcesses = toPartialProcesses reconciliations
                    , reconciliations = reconciliations
                    , calendar = Tuple.first caledit
                    , identifiers = getIdentifiers s.state (Type.TType TType.Event) uuid realType False
                    , values = getValues s.state.types s.state.valueTypes s.state.values (Type.TType TType.Event) uuid realType False
                    , gsubmodel = editgroups
                  }
                , Effect.batch [ closeMenu f s.menu, Effect.map GroupMsg (Effect.fromCmd editcmd) ]
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
        SelectType muuid ->
            let
                met =
                    muuid |> Maybe.andThen (\uid -> Dict.get (Uuid.toString uid) s.state.eventTypes)
            in
            ( { model
                | type_ = muuid
                , eventType = met
                , provider =
                    chooseIfSingleton
                        (s.state.agents
                            |> Dict.filter (\_ a -> met |> Maybe.map (\et -> containsScope s.state.types (IsItem (Type.TType a.what) a.uuid) et.providers) |> Maybe.withDefault True)
                            |> Dict.map (\_ a -> a.uuid)
                            |> Dict.values
                        )
                , receiver =
                    chooseIfSingleton
                        (s.state.agents
                            |> Dict.filter (\_ a -> met |> Maybe.map (\et -> containsScope s.state.types (IsItem (Type.TType a.what) a.uuid) et.receivers) |> Maybe.withDefault True)
                            |> Dict.map (\_ a -> a.uuid)
                            |> Dict.values
                        )
                , qty = Maybe.map .qty met
                , flow =
                    chooseIfSingleton
                        ((s.state.resources
                            |> Dict.filter (\_ r -> met |> Maybe.map (\ct -> containsScope s.state.types (IsItem (Type.TType r.what) r.uuid) ct.flowscope) |> Maybe.withDefault True)
                            |> Dict.map (\_ r -> ResourceFlow r)
                            |> Dict.values
                         )
                            ++ (s.state.resourceTypes
                                    |> Dict.filter (\_ rt -> met |> Maybe.map (\ct -> containsScope s.state.types (IsItem (Type.HType rt.what) rt.uuid) ct.flowscope) |> Maybe.withDefault True)
                                    |> Dict.map (\_ rt -> ResourceTypeFlow rt)
                                    |> Dict.values
                               )
                        )
                , identifiers = getIdentifiers s.state (Type.TType TType.Event) model.uuid muuid True
                , values = getValues s.state.types s.state.valueTypes s.state.values (Type.TType TType.Event) model.uuid muuid True
              }
            , Effect.none
            )

        SelectProcesses uuids ->
            let
                reconciliations =
                    uuids
                        |> List.map
                            (\p ->
                                let
                                    r =
                                        { qty = Rational.zero, process = p, event = model.uuid }
                                in
                                ( Reconcile.compare r, r )
                            )
                        |> Dict.fromList
            in
            ( { model
                | processes = uuids |> List.map (\u -> ( Uuid.toString u, u )) |> Dict.fromList
                , reconciliations = reconciliations
                , partialProcesses = toPartialProcesses reconciliations
              }
            , Effect.none
            )

        SelectProcessTypes pts ->
            ( { model | processTypes = pts |> List.map (\pt -> ( Uuid.toString pt, pt )) |> Dict.fromList }, Effect.none )

        SelectProvider uuid ->
            ( { model | provider = uuid }, Effect.none )

        SelectReceiver uuid ->
            ( { model | receiver = uuid }, Effect.none )

        InputQty qty ->
            ( { model | qty = Just qty }, Effect.none )

        InputFlow flow ->
            ( { model | flow = flow }, Effect.none )

        InputPartialProcesses partialProcesses ->
            ( { model
                | partialProcesses = partialProcesses
                , reconciliations = fromPartialProcesses model.uuid partialProcesses
              }
            , Effect.none
            )

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
            case validate s.state model of
                Ok e ->
                    let
                        qty =
                            Eval.exeval s.state { context = ( Type.TType TType.Event, model.uuid ) } s.state.values e.qty
                    in
                    ( model
                    , Effect.batch
                        [ Shared.dispatchMany s
                            (Payload.AddedEvent e
                                :: List.map Payload.AddedIdentifier (Dict.values model.identifiers)
                                ++ List.map Payload.AddedValue (Dict.values model.values)
                                ++ List.map (\uuid -> Payload.Grouped (Link (Type.TType TType.Event) e.uuid uuid)) (Dict.values <| Group.Input.added model.gsubmodel)
                                ++ List.map (\uuid -> Payload.Ungrouped (Link (Type.TType TType.Event) e.uuid uuid)) (Dict.values <| Group.Input.removed model.gsubmodel)
                                ++ (if Dict.isEmpty model.processes then
                                        Dict.values model.processTypes
                                            |> List.foldl (Shared.uuidAggregator model.seed) []
                                            |> List.concatMap
                                                (\( nextUuid, _, pt ) ->
                                                    Result.map
                                                        (\rational ->
                                                            [ Payload.AddedProcess <| Process TType.Process nextUuid pt
                                                            , Payload.Reconciled <| Reconciliation rational e.uuid nextUuid
                                                            ]
                                                        )
                                                        qty
                                                        |> Result.withDefault []
                                                )

                                    else
                                        List.map (\r -> Payload.Reconciled r) <| Dict.values model.reconciliations
                                   )
                            )

                        -- renew the Seed to avoid conflict due to uuidAggregator
                        , Effect.fromCmd <| Message.renewSeed ()
                        , Effect.fromCmd <| redirect s.navkey (Route.Entity Route.Event (Route.View { uuid = Uuid.toString model.uuid, type_ = Maybe.map Uuid.toString model.type_ }))
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))


view : Model -> View Msg
view model =
    { title = "Adding an Event"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


checkStep : State -> Model -> Result String ()
checkStep s model =
    case model.step of
        Step StepType ->
            checkMaybe model.type_ "You must select an Event Type" |> Result.map (\_ -> ())

        Step StepProvider ->
            checkMaybe model.provider "You must select a Provider" |> Result.map (\_ -> ())

        Step StepReceiver ->
            checkMaybe model.receiver "You must select a Receiver" |> Result.map (\_ -> ())

        Step StepFlow ->
            checkMaybe model.flow "You must input a Resource or Resource Type Flow"
                |> Result.andThen (\_ -> Eval.checkEval s { context = ( Type.TType TType.Event, model.uuid ) } s.values model.qty)
                |> Result.map (\_ -> ())

        Step StepReconcile ->
            checkAllOk (Tuple.second >> Rational.fromString >> Result.map (always ())) model.partialProcesses

        Step StepProcessTypes ->
            checkEmptyDict model.processTypes "You must choose at least one Process Types" |> Result.map (\_ -> ())

        Step _ ->
            Ok ()


validate : State -> Model -> Result String Event
validate s m =
    -- similar to haskell: f <$> a <*> b <*> c <*> d ...
    -- we apply multiple partial applications until we have the full value
    Result.map
        (Event TType.Event m.uuid (DateTime.View.toPosix m.calendar))
        (Eval.checkEval s { context = ( Type.TType TType.Event, m.uuid ) } s.values m.qty)
        |> andMapR (checkMaybe m.type_ "You must select an Event Type")
        |> andMapR (checkMaybe m.provider "You must select a Provider")
        |> andMapR (checkMaybe m.receiver "You must select a Receiver")
        |> andMapR (checkMaybe m.flow "You must input a Resource or Resource Type Flow")


inputPartialProcess : State -> List ( Uuid, RationalInput ) -> Int -> ( Uuid, RationalInput ) -> Element Msg
inputPartialProcess s partialProcesses index ( process, input ) =
    -- send an event to replace the partialProcess at index in the partialProcesses
    row [ spacing 5 ]
        [ RationalInput.inputText Rational.fromString
            (String.fromInt index)
            (Just "amount")
            (\str ->
                InputPartialProcesses
                    (partialProcesses
                        |> List.indexedMap
                            (\i partialProcess ->
                                if i == index then
                                    ( process, str )

                                else
                                    partialProcess
                            )
                    )
            )
            input
        , text <| "for: " ++ displayZone s SmallcardZone (Type.TType TType.Process) process
        ]


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepProcessTypes ->
                    multiSelect model
                        { inputMsg = SelectProcessTypes
                        , selection = .processTypes >> Dict.values
                        , title = "Process Types: "
                        , description = "Select all the Process Types related to this new Event"
                        , toString = displayZone s.state SmallcardZone (Type.HType HType.ProcessType)
                        , empty = "There are no Process Types yet to choose from"
                        , toDesc = always ""
                        , height = 50
                        , input = \_ _ _ -> none
                        }
                    <|
                        List.map .uuid <|
                            Dict.values s.state.processTypes

                Step.Step StepProcess ->
                    multiSelect model
                        { inputMsg = SelectProcesses
                        , selection = .processes >> Dict.values
                        , title = "Processes: "
                        , description = "Select the processes that correspond to this new event. Leave empty to create a new process."
                        , toString = displayZone s.state SmallcardZone (Type.TType TType.Process)
                        , empty = "There are no processes yet to choose from"
                        , toDesc = always ""
                        , height = 50
                        , input = \_ _ _ -> none
                        }
                        -- only choose among relevant processes whose type is in the ProcessType
                        (s.state.processes
                            |> Dict.filter (\_ p -> Dict.member (Uuid.toString p.type_) model.processTypes)
                            |> Dict.values
                            |> List.map .uuid
                        )

                Step.Step StepType ->
                    flatSelect s.state
                        { what = Type.HType HType.EventType
                        , muuid = model.type_
                        , onInput = SelectType
                        , title = "Type:"
                        , explain = "Select the type of the new Event:"
                        , empty = "(There are no Event Types yet to choose from)"
                        }
                        (model.processTypes
                            |> Dict.values
                            |> List.filterMap (\pt -> Dict.get (Uuid.toString pt) s.state.processTypes)
                            |> List.map .eventTypes
                            |> List.foldl Dict.union Dict.empty
                        )

                Step.Step StepProvider ->
                    Maybe.map
                        (\et ->
                            column [ spacing 20 ]
                                [ flatSelect s.state
                                    { what = Type.TType TType.Agent
                                    , muuid = model.provider
                                    , onInput = SelectProvider
                                    , title = "Provider:"
                                    , explain = "Select the provider:"
                                    , empty = "(There are no agents yet to choose from)"
                                    }
                                    (s.state.agents
                                        |> Dict.filter (\_ a -> containsScope s.state.types (IsItem (Type.TType a.what) a.uuid) et.providers)
                                        |> Dict.map (\_ a -> a.uuid)
                                    )
                                ]
                        )
                        model.eventType
                        |> Maybe.withDefault (text "This event has no type")

                Step.Step StepReceiver ->
                    Maybe.map
                        (\et ->
                            column [ spacing 20 ]
                                [ flatSelect s.state
                                    { what = Type.TType TType.Agent
                                    , muuid = model.receiver
                                    , onInput = SelectReceiver
                                    , title = "Receiver:"
                                    , explain = "Select the receiver:"
                                    , empty = "(There are no agents yet to choose from)"
                                    }
                                    (s.state.agents
                                        |> Dict.filter (\_ a -> containsScope s.state.types (IsItem (Type.TType a.what) a.uuid) et.receivers)
                                        |> Dict.map (\_ a -> a.uuid)
                                    )
                                ]
                        )
                        model.eventType
                        |> Maybe.withDefault (text "This event has no type")

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
                                        , context = ( Type.TType TType.Event, model.uuid )
                                        }
                                        s.state
                                        ( [], qty )
                                        qty
                                    , el [ padding 3, height (px 49), Border.width 2, Border.color color.item.border ] chosen
                                    ]
                                , choice
                                ]
                        )
                        model.eventType
                        model.qty
                        |> Maybe.withDefault (text "This event has no type")

                Step.Step StepReconcile ->
                    column [ spacing 20 ]
                        [ h1 "Allocation"
                        , p "What amount do you allocate on each process?"
                        , column [] <| List.indexedMap (\i pp -> inputPartialProcess s.state model.partialProcesses i pp) model.partialProcesses
                        ]

                Step.Step StepDate ->
                    column [ spacing 20, width fill ]
                        [ h1 "Select a date for the event"
                        , Element.map CalendarMsg <| DateTime.View.inputDate s model.calendar
                        ]

                Step.Step StepGroups ->
                    Element.map GroupMsg <| inputGroups { type_ = Type.TType TType.Event, mpuuid = model.type_ } s.state model.gsubmodel

                Step.Step StepIdentifiers ->
                    inputIdentifiers { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputIdentifier } model.identifiers

                Step.Step StepValues ->
                    inputValues
                        { onEnter = Step.nextMsg model Button Step.NextPage Step.Added
                        , onInput = InputValue
                        , context = ( Type.TType TType.Event, model.uuid )
                        }
                        s.state
                        model.values
    in
    floatingContainer s
        (Just <| Button Step.Cancel)
        "Adding an Event"
        (List.map (Element.map Button) (buttons model (checkStep s.state model)))
        [ step
        ]
