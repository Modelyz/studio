module Event.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Date
import DateTime.View
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Border as Border
import Event.Event exposing (Event)
import EventType.EventType exposing (EventType)
import Expression exposing (Expression)
import Expression.Input
import Expression.Rational as Rational
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
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Reconcile exposing (Reconciliation)
import Random.Pcg.Extended as Random exposing (Seed)
import Route exposing (Route, redirect)
import Scope as Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Shared
import Spa.Page
import Time exposing (millisToPosix)
import Type
import Typed.Type as TType
import Util exposing (applyR, checkMaybe, chooseIfSingleton, third)
import Value.Input exposing (inputValues)
import Value.Valuable exposing (getValues)
import Value.Value as Value exposing (Value)
import View exposing (..)
import View.FlatSelect exposing (flatSelect)
import View.MultiSelect exposing (multiSelect)
import View.Step as Step exposing (Step(..), buttons)
import View.Style exposing (color)
import Zone.View exposing (displayZone)
import Zone.Zone exposing (Zone(..))


typedConstructor : TType.Type
typedConstructor =
    TType.Event


hereType : Type.Type
hereType =
    Type.TType TType.Event


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
    , processes : List Uuid
    , type_ : Maybe Uuid
    , eventType : Maybe EventType
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
    = StepProcess
    | StepType
    | StepProvider
    | StepReceiver
    | StepFlow
    | StepDate
    | StepIdentifiers
    | StepValues
    | StepGroups


type Msg
    = SelectType (Maybe Uuid)
    | SelectProcesses (List Uuid)
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
        Route.Entity Route.Event (Route.Add tuuid) ->
            Just { route = route, uuid = Nothing, tuuid = tuuid }

        Route.Entity Route.Event (Route.Edit uuid tuuid) ->
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
            , processes = []
            , type_ = wantedType
            , eventType = met
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
            , qty = met |> Maybe.map .qty
            , calendar = calinit
            , uuid = newUuid
            , seed = newSeed
            , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType newUuid wantedType True
            , values = getValues s.state.types s.state.valueTypes s.state.values hereType newUuid wantedType True
            , gsubmodel = initgroups
            , warning = ""
            , step = Step.Step StepType
            , steps = [ Step.Step StepType, Step.Step StepProcess, Step.Step StepProvider, Step.Step StepReceiver, Step.Step StepFlow, Step.Step StepDate, Step.Step StepIdentifiers, Step.Step StepValues, Step.Step StepGroups ]
            }
    in
    f.uuid
        |> Maybe.map
            (\uuid ->
                let
                    realType =
                        Dict.get (Uuid.toString uuid) s.state.types |> Maybe.andThen third

                    reconciliations =
                        Dict.values <| Dict.filter (\_ r -> r.event == uuid) s.state.reconciliations

                    gs =
                        Group.groupsOf s.state.grouped uuid |> List.map (\i -> ( Uuid.toString i, i )) |> Dict.fromList

                    ( editgroups, editcmd ) =
                        Group.Input.init s gs

                    event =
                        Dict.get (Uuid.toString uuid) s.state.events

                    caledit =
                        event |> Maybe.map .when |> Maybe.withDefault (millisToPosix 0) |> Date.fromPosix s.zone |> DateTime.View.init False s.zone
                in
                ( { adding
                    | processes = reconciliations |> List.map .process
                    , type_ = realType
                    , uuid = uuid
                    , eventType = realType |> Maybe.andThen (\puuid -> Dict.get (Uuid.toString puuid) s.state.eventTypes)
                    , provider = event |> Maybe.map .provider
                    , receiver = event |> Maybe.map .receiver
                    , qty = event |> Maybe.map .qty
                    , flow = event |> Maybe.map .flow
                    , calendar = Tuple.first caledit
                    , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType uuid realType False
                    , values = getValues s.state.types s.state.valueTypes s.state.values hereType uuid realType False
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
        SelectType mh ->
            let
                met =
                    mh |> Maybe.andThen (\uid -> Dict.get (Uuid.toString uid) s.state.eventTypes)
            in
            ( { model
                | type_ = mh
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
                , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType model.uuid mh True
                , values = getValues s.state.types s.state.valueTypes s.state.values hereType model.uuid mh True
              }
            , Effect.none
            )

        SelectProcesses uuids ->
            ( { model | processes = uuids }, Effect.none )

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
                Ok e ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatchMany s
                            (Message.AddedEvent e
                                :: List.map Message.AddedIdentifier (Dict.values model.identifiers)
                                ++ List.map Message.AddedValue (Dict.values model.values)
                                ++ List.map (\uuid -> Message.Grouped (Link hereType e.uuid uuid)) (Dict.values <| Group.Input.added model.gsubmodel)
                                ++ List.map (\uuid -> Message.Ungrouped (Link hereType e.uuid uuid)) (Dict.values <| Group.Input.removed model.gsubmodel)
                                ++ List.map (\uuid -> Message.Reconciled (Reconciliation Rational.zero model.uuid uuid)) model.processes
                            )
                        , redirect s.navkey (Route.Entity Route.Event (Route.View (Uuid.toString model.uuid) (Maybe.map Uuid.toString model.type_))) |> Effect.fromCmd
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


checkStep : Model -> Result String ()
checkStep model =
    case model.step of
        Step StepProcess ->
            Ok ()

        Step StepType ->
            checkMaybe model.type_ "You must select an Event Type" |> Result.map (\_ -> ())

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


validate : Model -> Result String Event
validate m =
    -- similar to haskell: f <$> a <*> b <*> c <*> d ...
    -- we apply multiple partial applications until we have the full value
    Result.map
        (Event typedConstructor m.uuid (DateTime.View.toPosix m.calendar))
        (checkMaybe m.qty "The quantity is invalid")
        |> applyR (checkMaybe m.type_ "You must select an Event Type")
        |> applyR (checkMaybe m.provider "You must select a Provider")
        |> applyR (checkMaybe m.receiver "You must select a Receiver")
        |> applyR (checkMaybe m.flow "You must input a Resource or Resource Type Flow")


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepProcess ->
                    multiSelect model
                        { inputMsg = SelectProcesses
                        , selection = .processes
                        , title = "Processes: "
                        , description = "Select the processes that correspond to this new event. (If you don't know, click Next)"
                        , toString = displayZone s.state SmallcardTitle (Type.TType TType.Process)
                        , toDesc = always ""
                        , height = 50
                        , input = \_ _ _ -> none
                        }
                    <|
                        List.map .uuid <|
                            Dict.values s.state.processes

                {- what = Type.TType TType.Process
                   , muuids = model.processes
                   , onInput = SelectProcess
                   , title = "Process:"
                   , explain = "Choose the process of the new Event:"
                   , empty = "(There are no Processes yet to choose from)"
                   }
                   (s.state.processes |> Dict.map (\_ a -> a.uuid))
                -}
                Step.Step StepType ->
                    flatSelect s
                        { what = Type.HType HType.EventType
                        , muuid = model.type_
                        , onInput = SelectType
                        , title = "Type:"
                        , explain = "Select the type of the new Event:"
                        , empty = "(There are no Event Types yet to choose from)"
                        }
                        (s.state.eventTypes |> Dict.map (\_ a -> a.uuid))

                Step.Step StepProvider ->
                    Maybe.map
                        (\et ->
                            column [ spacing 20 ]
                                [ flatSelect s
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
                                [ flatSelect s
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
                                        , context = ( hereType, model.uuid )
                                        }
                                        s
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

                Step.Step StepDate ->
                    column [ spacing 20, width fill ]
                        [ h1 "Select a date for the event"
                        , Element.map CalendarMsg <| DateTime.View.inputDate s model.calendar
                        ]

                Step.Step StepGroups ->
                    Element.map GroupMsg <| inputGroups { type_ = hereType, mpuuid = model.type_ } s model.gsubmodel

                Step.Step StepIdentifiers ->
                    inputIdentifiers { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputIdentifier } model.identifiers

                Step.Step StepValues ->
                    inputValues
                        { onEnter = Step.nextMsg model Button Step.NextPage Step.Added
                        , onInput = InputValue
                        , context = ( Type.TType TType.Event, model.uuid )
                        }
                        s
                        model.values
    in
    floatingContainer s
        (Just <| Button Step.Cancel)
        "Adding an Event"
        (List.map (Element.map Button) (buttons model (checkStep model)))
        [ step
        ]
