module Group.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
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
import Scope as Scope exposing (Scope)
import Scope.View exposing (selectScope)
import Shared
import Spa.Page
import Type
import Typed.Type as TType
import Util exposing (third)
import Value.Input exposing (inputValues)
import Value.Valuable exposing (getValues)
import Value.Value as Value exposing (Value)
import View exposing (..)
import View.FlatSelect exposing (flatSelect)
import View.Step as Step exposing (Step(..), buttons)


type alias TypedType =
    Group


typedConstructor : TType.Type
typedConstructor =
    TType.Group


hereType : Type.Type
hereType =
    Type.TType TType.Group


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
    , parent : Maybe Uuid
    , scope : Scope
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , gsubmodel : Group.Input.Model
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepType
    | StepParent
    | StepScope
    | StepIdentifiers
    | StepValues
    | StepGroups


type Msg
    = InputType (Maybe Uuid)
    | InputParent (Maybe Uuid)
    | InputIdentifier Identifier
    | InputValue Value
    | InputScope Scope
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
        Route.Entity Route.Group (Route.Add tuuid) ->
            Just { route = route, uuid = Nothing, tuuid = tuuid }

        Route.Entity Route.Group (Route.Edit uuid tuuid) ->
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

        adding =
            { route = f.route
            , isNew = isNew
            , type_ = wantedType
            , parent = Nothing
            , scope = Scope.anything
            , uuid = newUuid
            , seed = newSeed
            , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType newUuid wantedType True
            , values = getValues s.state.types s.state.valueTypes s.state.values hereType newUuid wantedType True
            , gsubmodel = initgroups
            , warning = ""
            , step = Step.Step StepType
            , steps = [ Step.Step StepType, Step.Step StepScope, Step.Step StepIdentifiers, Step.Step StepParent, Step.Step StepValues, Step.Step StepGroups ]
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

                    ( editgroups, editcmd ) =
                        Group.Input.init s gs

                    group =
                        Dict.get (Uuid.toString uuid) s.state.groups

                    scope =
                        Maybe.map .scope group |> Maybe.withDefault Scope.anything
                in
                ( { adding
                    | type_ = realType
                    , parent = Maybe.andThen .parent group
                    , scope = scope
                    , uuid = uuid
                    , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType uuid realType False
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
                , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType model.uuid mh True
                , values = getValues s.state.types s.state.valueTypes s.state.values hereType model.uuid mh True
              }
            , Effect.none
            )

        InputParent parent ->
            ( { model | parent = parent }, Effect.none )

        InputScope scope ->
            ( { model | scope = scope }, Effect.none )

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
                            (Message.DefinedGroup t
                                :: List.map Message.AddedIdentifier (Dict.values model.identifiers)
                                ++ List.map Message.AddedValue (Dict.values model.values)
                                ++ List.map (\uuid -> Message.Grouped (Link hereType t.uuid uuid)) (Dict.values <| Group.Input.added model.gsubmodel)
                                ++ List.map (\uuid -> Message.Ungrouped (Link hereType t.uuid uuid)) (Dict.values <| Group.Input.removed model.gsubmodel)
                            )
                        , redirect s.navkey (Route.Entity Route.Group (Route.View (Uuid.toString model.uuid) (Maybe.map Uuid.toString model.type_))) |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))


view : Model -> View Msg
view model =
    { title = "Adding a Group"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


checkStep : Model -> Result String ()
checkStep model =
    case model.step of
        Step StepType ->
            Maybe.map (\_ -> Ok ()) model.type_ |> Maybe.withDefault (Err "You must select a Group Type")

        Step StepIdentifiers ->
            Ok ()

        Step StepValues ->
            Ok ()

        Step StepGroups ->
            Ok ()

        Step.Step StepScope ->
            Ok ()

        Step.Step StepParent ->
            Ok ()


validate : Model -> Result String TypedType
validate m =
    case m.type_ of
        Just uuid ->
            -- TODO check that TType thing is useful
            Ok <| Group typedConstructor m.uuid uuid m.parent m.scope

        Nothing ->
            Err "You must select a Group Type"


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepType ->
                    flatSelect s
                        { what = Type.HType HType.GroupType
                        , muuid = model.type_
                        , onInput = InputType
                        , title = "Type:"
                        , explain = "Choose the type of the new group:"
                        , empty = "(There are no Group Types yet to choose from)"
                        }
                        (s.state.groupTypes |> Dict.map (\_ a -> a.uuid))

                Step.Step StepParent ->
                    flatSelect s
                        { what = Type.TType TType.Group
                        , muuid = model.parent
                        , onInput = InputParent
                        , title = "Parent group:"
                        , explain = "Choose the parent of the new group:"
                        , empty = "(There are no Groups yet to choose from)"
                        }
                        -- we display the groups whose type is an ascendent of the newly added group
                        ((s.state.groups |> Dict.filter (\_ v -> Maybe.map (Type.hasCommonParent s.state.types v.type_) model.type_ |> Maybe.withDefault False)) |> Dict.map (\_ a -> a.uuid))

                Step.Step StepScope ->
                    selectScope s.state InputScope model.scope Scope.anything "What can be in the group?"

                Step.Step StepGroups ->
                    Element.map GroupMsg <| inputGroups { type_ = hereType, mpuuid = model.type_ } s model.gsubmodel

                Step.Step StepIdentifiers ->
                    inputIdentifiers { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputIdentifier } model.identifiers

                Step.Step StepValues ->
                    inputValues { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputValue, context = ( hereType, model.uuid ) } s model.values
    in
    floatingContainer s
        (Just <| Button Step.Cancel)
        "Adding a Group"
        (List.map (Element.map Button) (buttons model (checkStep model)))
        [ step
        ]
