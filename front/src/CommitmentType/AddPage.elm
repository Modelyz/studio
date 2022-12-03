module CommitmentType.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import CommitmentType.CommitmentType exposing (CommitmentType)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Expression as Expression exposing (Expression(..))
import Expression.Editor exposing (view)
import Flow exposing (Flow)
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
import Scope.View exposing (selectScope)
import Shared
import Spa.Page
import Type
import Typed.Type as TType
import Util exposing (checkEmptyString, checkListOne, checkMaybe, third)
import Value.Input exposing (inputValues)
import Value.Valuable exposing (getValues)
import Value.Value as Value exposing (Value)
import View exposing (..)
import View.FlatSelect exposing (flatSelect)
import View.Step as Step exposing (Step(..), buttons)


hereType : Type.Type
hereType =
    Type.HType HType.CommitmentType


hierarchicConstructor =
    HType.CommitmentType


constructor =
    CommitmentType


type alias Flags =
    { route : Route, uuid : Maybe Uuid }


type alias Model =
    { route : Route
    , isNew : Bool
    , uuid : Uuid
    , seed : Seed
    , type_ : Maybe Uuid
    , providers : Scope
    , receivers : Scope
    , flowscope : Scope
    , editor : Expression.Editor.Model
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
    | StepIdentifiers
    | StepProviders
    | StepReceivers
    | StepFlow
    | StepValues
    | StepGroups


type Msg
    = SelectType (Maybe Uuid)
    | SelectProviders Scope
    | SelectReceivers Scope
    | SelectFlow Scope
    | InputIdentifier Identifier
    | InputGroups (Dict String Uuid)
    | InputValue Value
    | Button Step.Msg
    | EditorMsg Expression.Editor.Msg


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
        Route.Entity Route.CommitmentType (Route.Add _) ->
            Just { route = route, uuid = Nothing }

        Route.Entity Route.CommitmentType (Route.Edit uuid _) ->
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
            , providers = Scope.empty
            , receivers = Scope.empty
            , flowscope = Scope.empty
            , editor = Expression.Editor.init s (HasType (Type.TType TType.Commitment)) []
            , uuid = newUuid
            , seed = newSeed
            , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType newUuid Nothing True
            , values = getValues s.state.types s.state.valueTypes s.state.values hereType newUuid Nothing True
            , oldGroups = Dict.empty
            , groups = Dict.empty
            , warning = ""
            , step = Step.Step StepType
            , steps =
                [ Step.Step StepType
                , Step.Step StepIdentifiers
                , Step.Step StepValues
                , Step.Step StepProviders
                , Step.Step StepReceivers
                , Step.Step StepFlow
                , Step.Step StepGroups
                ]
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

                    type_ =
                        Dict.get (Uuid.toString uuid) s.state.types |> Maybe.andThen (\( _, _, x ) -> x)

                    ct =
                        Dict.get (Uuid.toString uuid) s.state.commitmentTypes

                    flowscope =
                        Maybe.map .flowscope ct |> Maybe.withDefault (HasType (Type.TType TType.Commitment))
                in
                { adding
                    | type_ = type_
                    , uuid = uuid
                    , providers = Maybe.map .providers ct |> Maybe.withDefault Scope.empty
                    , receivers = Maybe.map .receivers ct |> Maybe.withDefault Scope.empty
                    , flowscope = flowscope
                    , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType uuid type_ False
                    , values = getValues s.state.types s.state.valueTypes s.state.values hereType uuid type_ False
                    , oldGroups = oldGroups
                    , groups = oldGroups
                    , editor =
                        Expression.Editor.init s
                            flowscope
                            (ct |> Maybe.map (.qty >> List.singleton) |> Maybe.withDefault [])
                }
            )
        |> Maybe.withDefault adding
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        SelectType mh ->
            ( { model
                | type_ = mh
                , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType model.uuid mh True
                , values = getValues s.state.types s.state.valueTypes s.state.values hereType model.uuid mh True
                , editor = Expression.Editor.init s (Maybe.map (HasUserType (Type.TType TType.Commitment)) mh |> Maybe.withDefault (HasType (Type.TType TType.Commitment))) []
              }
            , Effect.none
            )

        SelectProviders scope ->
            ( { model | providers = scope }, Effect.none )

        SelectReceivers scope ->
            ( { model | receivers = scope }, Effect.none )

        SelectFlow scope ->
            ( { model
                | flowscope = scope
              }
            , Effect.none
            )

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
                            (Message.AddedCommitmentType t
                                :: List.map Message.AddedIdentifier (Dict.values model.identifiers)
                                ++ List.map Message.AddedValue (Dict.values model.values)
                                ++ List.map (\uuid -> Message.Grouped (Link hereType t.uuid uuid)) (Dict.values addedGroups)
                                ++ List.map (\uuid -> Message.Ungrouped (Link hereType t.uuid uuid)) (Dict.values removedGroups)
                            )
                        , redirect s.navkey (Route.Entity Route.CommitmentType (Route.View (Uuid.toString model.uuid) Nothing)) |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))

        EditorMsg editormsg ->
            Expression.Editor.update s editormsg model.editor
                |> (\( x, y ) -> ( { model | editor = x }, Effect.map EditorMsg <| Effect.fromCmd y ))


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding an Commitment Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


checkStep : Model -> Result String ()
checkStep model =
    case model.step of
        Step StepType ->
            Ok ()

        Step StepProviders ->
            Ok ()

        Step StepReceivers ->
            Ok ()

        Step StepFlow ->
            Ok ()

        Step StepIdentifiers ->
            Ok ()

        Step StepValues ->
            Ok ()

        Step StepGroups ->
            Ok ()


validate : Model -> Result String CommitmentType
validate m =
    Result.map
        (constructor hierarchicConstructor m.uuid m.type_ m.providers m.receivers m.flowscope)
        (Expression.Editor.checkExpression m.editor)


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
                        , title = "Parent Type:"
                        , explain = "You can choose among the following types:"
                        , empty = "(There are no Commitment Types yet to choose from)"
                        }
                        (s.state.commitmentTypes |> Dict.map (\_ a -> a.uuid))

                Step.Step StepProviders ->
                    selectScope s SelectProviders model.providers (Scope.HasType (Type.TType TType.Agent)) "Provider Agents:"

                Step.Step StepReceivers ->
                    selectScope s SelectReceivers model.receivers (Scope.HasType (Type.TType TType.Agent)) "Receiver Agents:"

                Step.Step StepFlow ->
                    column [ alignTop, width <| minimum 200 fill, spacing 10 ]
                        [ selectScope s SelectFlow model.flowscope (Scope.or (Scope.HasType (Type.TType TType.Resource)) (Scope.HasType (Type.HType HType.ResourceType))) "What can be exchanged:"
                        , h2 "Build an expression for the quantity exchanged:"
                        , Element.map EditorMsg <| Expression.Editor.view s model.editor
                        ]

                Step.Step StepGroups ->
                    inputGroups { onInput = InputGroups, type_ = hereType, mpuuid = model.type_ } s model.groups

                Step.Step StepIdentifiers ->
                    inputIdentifiers { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputIdentifier } model.identifiers

                Step.Step StepValues ->
                    inputValues { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputValue, context = ( hereType, model.uuid ) } s model.values
    in
    floatingContainer2 s
        (Just <| Button Step.Cancel)
        "Adding an Commitment Type"
        (List.map (Element.map Button) (buttons model (checkStep model)))
        [ step
        ]
        (Maybe.map (Element.map EditorMsg) <| Expression.Editor.viewSubpage s model.editor)
