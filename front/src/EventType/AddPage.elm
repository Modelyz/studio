module EventType.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Configuration as Config exposing (Configuration(..))
import Configuration.Zone exposing (Zone(..))
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import EventType.EventType exposing (EventType)
import Expression exposing (Expression(..))
import Expression.Editor exposing (view)
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
import View.Style exposing (size)


hereType : Type.Type
hereType =
    Type.HType HType.EventType


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
    , resources : Scope
    , editor : Expression.Editor.Model
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , gsubmodel : Group.Input.Model
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    , hadMenu : Bool
    , isMenu : Bool
    , createResource : Bool
    }


type Step
    = StepType
    | StepIdentifiers
    | StepProviders
    | StepReceivers
    | StepFlow
    | StepValues
    | StepGroups
    | StepOptions


type Msg
    = SelectType (Maybe Uuid)
    | SelectProviders Scope
    | SelectReceivers Scope
    | SelectResources Scope
    | InputIdentifier Identifier
    | InputIsMenu Bool
    | InputCreateResource Bool
    | GroupMsg Group.Input.Msg
    | InputValue Value
    | Button Step.Msg
    | EditorMsg Expression.Editor.Msg


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
        Route.Entity Route.EventType (Route.Add _) ->
            Just { route = route, uuid = Nothing }

        Route.Entity Route.EventType (Route.Edit p) ->
            Just { route = route, uuid = Uuid.fromString p.uuid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        ( newUuid, newSeed ) =
            Random.step Uuid.generator s.currentSeed

        isNew =
            f.uuid == Nothing

        ( initgroups, initcmd ) =
            Group.Input.init s Dict.empty

        adding =
            { route = f.route
            , isNew = isNew
            , type_ = Nothing
            , providers = Scope.HasType (Type.TType TType.Agent)
            , receivers = Scope.HasType (Type.TType TType.Agent)
            , resources = Scope.or (Scope.HasType (Type.TType TType.Resource)) (Scope.HasType (Type.HType HType.ResourceType))
            , editor = Expression.Editor.init (HasType (Type.TType TType.Event)) []
            , uuid = newUuid
            , seed = newSeed
            , identifiers = getIdentifiers s.state hereType newUuid Nothing True
            , values = getValues s.state.types s.state.valueTypes s.state.values hereType newUuid Nothing True
            , gsubmodel = initgroups
            , isMenu = True
            , createResource = False
            , hadMenu = True
            , warning = ""
            , step = Step.Step StepType
            , steps =
                [ Step.Step StepType
                , Step.Step StepIdentifiers
                , Step.Step StepValues
                , Step.Step StepProviders
                , Step.Step StepReceivers
                , Step.Step StepFlow
                , Step.Step StepOptions
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

                    gs =
                        Group.groupsOf s.state.grouped uuid |> List.map (\i -> ( Uuid.toString i, i )) |> Dict.fromList

                    hadMenu =
                        Config.onlyMenu s.state.configs
                            |> Dict.get (Config.compare (MenuDisplay { what = HType.EventType, uuid = uuid, isMenu = False }))
                            |> Maybe.map
                                (\config ->
                                    case config of
                                        MenuDisplay display ->
                                            display.isMenu

                                        _ ->
                                            True
                                )
                            |> Maybe.withDefault True

                    ( editgroups, editcmd ) =
                        Group.Input.init s gs

                    et =
                        Dict.get (Uuid.toString uuid) s.state.eventTypes

                    resources =
                        Maybe.map .resources et |> Maybe.withDefault (HasType (Type.TType TType.Event))
                in
                ( { adding
                    | type_ = realType
                    , uuid = uuid
                    , providers = Maybe.map .providers et |> Maybe.withDefault (Scope.HasType (Type.TType TType.Agent))
                    , receivers = Maybe.map .receivers et |> Maybe.withDefault (Scope.HasType (Type.TType TType.Agent))
                    , resources = resources
                    , editor =
                        Expression.Editor.init
                            resources
                            (et |> Maybe.map (.qty >> List.singleton) |> Maybe.withDefault [])
                    , identifiers = getIdentifiers s.state hereType uuid realType False
                    , values = getValues s.state.types s.state.valueTypes s.state.values hereType uuid realType False
                    , gsubmodel = editgroups
                    , hadMenu = hadMenu
                    , isMenu = hadMenu
                    , createResource = Maybe.map .createResource et |> Maybe.withDefault False
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
        SelectType mh ->
            ( { model
                | type_ = mh
                , identifiers = getIdentifiers s.state hereType model.uuid mh True
                , values = getValues s.state.types s.state.valueTypes s.state.values hereType model.uuid mh True
                , editor = Expression.Editor.init (Maybe.map (HasUserType (Type.TType TType.Event)) mh |> Maybe.withDefault (HasType (Type.TType TType.Event))) []
              }
            , Effect.none
            )

        SelectProviders scope ->
            ( { model | providers = scope }, Effect.none )

        SelectReceivers scope ->
            ( { model | receivers = scope }, Effect.none )

        SelectResources scope ->
            ( { model
                | resources = scope
              }
            , Effect.none
            )

        InputIdentifier i ->
            ( { model | identifiers = Dict.insert (Identifier.compare i) i model.identifiers }, Effect.none )

        InputValue v ->
            ( { model | values = Dict.insert (Value.compare v) v model.values }, Effect.none )

        InputIsMenu isMenu ->
            ( { model | isMenu = isMenu }, Effect.none )

        InputCreateResource createResource ->
            ( { model | createResource = createResource }, Effect.none )

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
                            (Payload.AddedEventType t
                                :: List.map Payload.AddedIdentifier (Dict.values model.identifiers)
                                ++ List.map Payload.AddedValue (Dict.values model.values)
                                ++ List.map (\uuid -> Payload.Grouped (Link hereType t.uuid uuid)) (Dict.values <| Group.Input.added model.gsubmodel)
                                ++ List.map (\uuid -> Payload.Ungrouped (Link hereType t.uuid uuid)) (Dict.values <| Group.Input.removed model.gsubmodel)
                                ++ (if model.hadMenu == model.isMenu then
                                        []

                                    else
                                        [ Payload.Configured <| MenuDisplay { what = HType.EventType, uuid = t.uuid, isMenu = model.isMenu } ]
                                   )
                            )
                        , redirect s.navkey (Route.Entity Route.EventType (Route.View { uuid = Uuid.toString model.uuid, type_ = Nothing })) |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))

        EditorMsg editormsg ->
            Expression.Editor.update editormsg model.editor
                |> (\( x, y ) -> ( { model | editor = x }, Effect.map EditorMsg <| Effect.fromCmd y ))


view : Model -> View Msg
view model =
    { title = "Adding an Event Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


checkStep : Model -> Result String ()
checkStep model =
    case model.step of
        Step StepFlow ->
            Result.map (\_ -> ()) <| Expression.Editor.checkExpression model.editor

        _ ->
            Ok ()


validate : Model -> Result String EventType
validate m =
    Result.map
        (EventType HType.EventType m.uuid m.type_ m.providers m.receivers m.resources m.createResource)
        (Expression.Editor.checkExpression m.editor)


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepType ->
                    flatSelect s.state
                        { what = hereType
                        , muuid = model.type_
                        , onInput = SelectType
                        , title = "Parent Type:"
                        , explain = "You can choose among the following types:"
                        , empty = "(There are no Event Types yet to choose from)"
                        , additional = Nothing
                        }
                        (s.state.eventTypes |> Dict.map (\_ a -> a.uuid))

                Step.Step StepProviders ->
                    selectScope s.state SelectProviders model.providers (Scope.HasType (Type.TType TType.Agent)) "Provider Agents:"

                Step.Step StepReceivers ->
                    selectScope s.state SelectReceivers model.receivers (Scope.HasType (Type.TType TType.Agent)) "Receiver Agents:"

                Step.Step StepFlow ->
                    column [ alignTop, width <| minimum 200 fill, spacing 10 ]
                        -- TODO review Scope.or and check Commitment as well
                        [ selectScope s.state SelectResources model.resources (Scope.HasType (Type.TType TType.Resource)) "What can be exchanged:"
                        , h2 "Build an expression for the quantity exchanged:"
                        , Element.map EditorMsg <| Expression.Editor.view s model.editor
                        ]

                Step.Step StepGroups ->
                    Element.map GroupMsg <| inputGroups { type_ = hereType, mpuuid = model.type_ } s.state model.gsubmodel

                Step.Step StepOptions ->
                    column [ alignTop, width <| minimum 200 fill, spacing 10 ]
                        [ h3 "Options:"
                        , column [ Font.size size.text.main, spacing 10 ]
                            [ Input.checkbox
                                []
                                { onChange = InputIsMenu
                                , icon = Input.defaultCheckbox
                                , checked = model.isMenu
                                , label = Input.labelRight [] <| text "Display as a menu item"
                                }
                            , Input.checkbox
                                []
                                { onChange = InputCreateResource
                                , icon = Input.defaultCheckbox
                                , checked = model.createResource
                                , label = Input.labelRight [] <| text "Create the resource with the event (for services)"
                                }
                            ]
                        ]

                Step.Step StepIdentifiers ->
                    inputIdentifiers { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputIdentifier } model.identifiers

                Step.Step StepValues ->
                    inputValues { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputValue, context = ( hereType, model.uuid ) } s.state model.values
    in
    floatingContainer2 s
        (Just <| Button Step.Cancel)
        "Adding an Event Type"
        (List.map (Element.map Button) (buttons model (checkStep model)))
        [ step
        ]
        (Maybe.map (Element.map EditorMsg) <| Expression.Editor.viewSubpage s model.editor)
