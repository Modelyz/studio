module Process.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Configuration.Zone exposing (Zone(..))
import Configuration.Zone.View exposing (displayZone)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Expression.Rational as Rational
import Expression.RationalInput as RationalInput exposing (RationalInput)
import Group.Group as Group
import Group.Input exposing (inputGroups)
import Group.Link exposing (Link)
import Hierarchy.Type as HType
import Ident.Identifiable exposing (getIdentifiers)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.Input exposing (inputIdentifiers)
import Payload
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process exposing (Process)
import Process.Reconcile as Reconcile exposing (Reconciliation, fromPartialEvents, toPartialEvents)
import Random.Pcg.Extended as Random exposing (Seed)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import State exposing (State)
import Type
import Typed.Type as TType
import Util exposing (checkAllOk, third)
import Value.Input exposing (inputValues)
import Value.Valuable exposing (getValues)
import Value.Value as Value exposing (Value)
import View exposing (..)
import View.FlatSelect exposing (flatSelect)
import View.MultiSelect exposing (multiSelect)
import View.Step as Step exposing (Step(..), buttons)


typedConstructor : TType.Type
typedConstructor =
    TType.Process


hereType : Type.Type
hereType =
    Type.TType TType.Process


type alias Flags =
    { route : Route
    , uuid : Maybe Uuid

    -- type transmitted through url
    , tuuid : Maybe String
    }


type alias Model =
    { route : Route
    , isNew : Bool
    , uuid : Uuid
    , seed : Seed
    , type_ : Maybe Uuid
    , partialEvents : List ( Uuid, RationalInput )
    , reconciliations : Dict String Reconciliation
    , oldReconciliations : Dict String Reconciliation
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , gsubmodel : Group.Input.Model
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepType
    | StepReconcile
    | StepIdentifiers
    | StepValues
    | StepGroups


type Msg
    = InputType (Maybe Uuid)
    | InputPartialEvents (List ( Uuid, RationalInput ))
    | InputIdentifier Identifier
    | InputValue Value
    | GroupMsg Group.Input.Msg
    | Button Step.Msg


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
        Route.Entity Route.Process (Route.Add p) ->
            Just { route = route, uuid = Nothing, tuuid = p.type_ }

        Route.Entity Route.Process (Route.Edit p) ->
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

        adding =
            { route = f.route
            , isNew = isNew
            , type_ = wantedType
            , partialEvents = []
            , oldReconciliations = Dict.empty
            , reconciliations = Dict.empty
            , uuid = newUuid
            , seed = newSeed
            , identifiers = getIdentifiers s.state hereType newUuid wantedType True
            , values = getValues s.state.types s.state.valueTypes s.state.values hereType newUuid wantedType True
            , gsubmodel = initgroups
            , warning = ""
            , step = Step.Step StepType
            , steps = [ Step.Step StepType, Step.Step StepReconcile, Step.Step StepIdentifiers, Step.Step StepValues, Step.Step StepGroups ]
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

                    oldReconciliations =
                        s.state.reconciliations |> Reconcile.byProcess uuid

                    ( editgroups, editcmd ) =
                        Group.Input.init s gs
                in
                ( { adding
                    | type_ = realType
                    , uuid = uuid
                    , partialEvents = toPartialEvents oldReconciliations
                    , oldReconciliations = oldReconciliations
                    , reconciliations = oldReconciliations
                    , identifiers = getIdentifiers s.state hereType uuid realType False
                    , values = getValues s.state.types s.state.valueTypes s.state.values hereType uuid realType False
                    , gsubmodel = editgroups
                  }
                , Effect.batch
                    [ closeMenu f s.menu
                    , Effect.map GroupMsg (Effect.fromCmd editcmd)
                    ]
                )
            )
        |> Maybe.withDefault
            ( adding
            , Effect.batch
                [ closeMenu f s.menu
                , Effect.map GroupMsg (Effect.fromCmd initcmd)
                ]
            )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputType mh ->
            ( { model
                | type_ = mh
                , identifiers = getIdentifiers s.state hereType model.uuid mh True
                , values = getValues s.state.types s.state.valueTypes s.state.values hereType model.uuid mh True
              }
            , Effect.none
            )

        InputPartialEvents partialEvents ->
            ( { model
                | partialEvents = partialEvents
                , reconciliations = fromPartialEvents model.uuid partialEvents
              }
            , Effect.none
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
                Ok t ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatchMany s
                            (Payload.AddedProcess t
                                :: List.map Payload.AddedIdentifier (Dict.values model.identifiers)
                                ++ List.map Payload.AddedValue (Dict.values model.values)
                                ++ List.map (\uuid -> Payload.Grouped (Link hereType t.uuid uuid)) (Dict.values <| Group.Input.added model.gsubmodel)
                                ++ List.map (\uuid -> Payload.Ungrouped (Link hereType t.uuid uuid)) (Dict.values <| Group.Input.removed model.gsubmodel)
                                ++ List.map Payload.Reconciled (Dict.values model.reconciliations)
                                ++ List.map Payload.Unreconciled (Dict.values <| Dict.diff model.oldReconciliations model.reconciliations)
                            )
                        , redirect s.navkey (Route.Entity Route.Process (Route.View { uuid = Uuid.toString model.uuid, type_ = Maybe.map Uuid.toString model.type_ })) |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))


view : Model -> View Msg
view model =
    { title = "Adding a Process"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


checkStep : Model -> Result String ()
checkStep model =
    case model.step of
        Step StepType ->
            Maybe.map (\_ -> Ok ()) model.type_ |> Maybe.withDefault (Err "You must select a Process Type")

        Step StepReconcile ->
            checkAllOk (Tuple.second >> Rational.fromString >> Result.map (always ())) model.partialEvents

        _ ->
            Ok ()


validate : Model -> Result String Process
validate m =
    case m.type_ of
        Just uuid ->
            -- TODO check that TType thing is useful
            Ok <| Process typedConstructor m.uuid uuid

        Nothing ->
            Err "You must select a Process Type"


inputPartialEvent : List ( Uuid, RationalInput ) -> Int -> ( Uuid, RationalInput ) -> Element Msg
inputPartialEvent partialEvents index ( event, input ) =
    RationalInput.inputText Rational.fromString
        (String.fromInt index)
        (Just "amount")
        (\str ->
            InputPartialEvents
                (partialEvents
                    |> List.indexedMap
                        (\i partialEvent ->
                            if i == index then
                                ( event, str )

                            else
                                partialEvent
                        )
                )
        )
        input



{- Input.text [ width (px 75), height shrink, padding 5, spacing 5 ]
   { onChange =
       \str ->
           InputPartialEvents
               (partialEvents
                   |> List.indexedMap
                       (\i partialEvent ->
                           if i == index then
                               ( event, str )

                           else
                               partialEvent
                       )
               )
   , text = input
   , placeholder = Just <| Input.placeholder [] <| text "amount"
   , label = Input.labelHidden "Reconciliation"
   }
-}


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepType ->
                    flatSelect s.state
                        { what = Type.HType HType.ProcessType
                        , muuid = model.type_
                        , onInput = InputType
                        , title = "Type:"
                        , explain = "Choose the type of the new Process:"
                        , empty = "(There are no Process Types yet to choose from)"
                        }
                        (s.state.processTypes |> Dict.map (\_ a -> a.uuid))

                Step.Step StepReconcile ->
                    multiSelect
                        model
                        { inputMsg = InputPartialEvents
                        , selection = .partialEvents
                        , title = "Events: "
                        , description = "Select the Events that are part of this process"
                        , toString = Tuple.first >> displayZone s.state SmallcardZone (Type.TType TType.Event)
                        , toDesc = always ""
                        , empty = "There are no Events yet to choose from"
                        , height = 100
                        , input = inputPartialEvent
                        }
                        -- TODO filter on unreconciled events
                        (Dict.values s.state.events
                            |> List.map (\e -> ( e.uuid, Rational.toFloatString Rational.zero ))
                        )

                Step.Step StepGroups ->
                    Element.map GroupMsg <| inputGroups { type_ = hereType, mpuuid = model.type_ } s.state model.gsubmodel

                Step.Step StepIdentifiers ->
                    inputIdentifiers { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputIdentifier } model.identifiers

                Step.Step StepValues ->
                    inputValues { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputValue, context = ( hereType, model.uuid ) } s.state model.values
    in
    floatingContainer s
        (Just <| Button Step.Cancel)
        "Adding a Process"
        (List.map (Element.map Button) (buttons model (checkStep model)))
        [ step
        ]
