module ProcessType.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Configuration as Config exposing (Configuration(..))
import Configuration.Zone exposing (Zone(..))
import Configuration.Zone.View exposing (displayZone)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Group.Group as Group
import Group.Input exposing (inputGroups)
import Group.Link exposing (Link)
import Hierarchy.Type as HType
import Ident.Identifiable exposing (getIdentifiers)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.Input exposing (inputIdentifiers)
import Payload
import Prng.Uuid as Uuid exposing (Uuid)
import ProcessType.ProcessType exposing (ProcessType)
import Random.Pcg.Extended as Random exposing (Seed)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import Type
import Util exposing (third)
import Value.Input exposing (inputValues)
import Value.Valuable exposing (getValues)
import Value.Value as Value exposing (Value)
import View exposing (..)
import View.FlatSelect exposing (flatSelect)
import View.MultiSelect exposing (multiSelect)
import View.Step as Step exposing (Step(..), buttons)
import View.Style exposing (size)


hereType : Type.Type
hereType =
    Type.HType HType.ProcessType


type alias Flags =
    { route : Route, uuid : Maybe Uuid }


type alias Model =
    { route : Route
    , isNew : Bool
    , uuid : Uuid
    , seed : Seed
    , type_ : Maybe Uuid
    , eventTypes : Dict String Uuid
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , gsubmodel : Group.Input.Model
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    , hadMenu : Bool
    , isMenu : Bool
    }


type Step
    = StepType
    | StepIdentifiers
    | StepValues
    | StepGroups
    | StepOptions
    | StepEventTypes


type Msg
    = InputType (Maybe Uuid)
    | InputIdentifier Identifier
    | InputIsMenu Bool
    | InputEventTypes (List Uuid)
    | GroupMsg Group.Input.Msg
    | InputValue Value
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
        Route.Entity Route.ProcessType (Route.Add _) ->
            Just { route = route, uuid = Nothing }

        Route.Entity Route.ProcessType (Route.Edit p) ->
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
            , eventTypes = Dict.empty
            , uuid = newUuid
            , seed = newSeed
            , identifiers = getIdentifiers s.state hereType newUuid Nothing True
            , values = getValues s.state.types s.state.valueTypes s.state.values hereType newUuid Nothing True
            , gsubmodel = initgroups
            , isMenu = True
            , hadMenu = True
            , warning = ""
            , step = Step.Step StepType
            , steps =
                [ Step.Step StepType
                , Step.Step StepIdentifiers
                , Step.Step StepEventTypes
                , Step.Step StepValues
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

                    eventTypes =
                        Dict.get (Uuid.toString uuid) s.state.processTypes |> Maybe.map .eventTypes |> Maybe.withDefault Dict.empty

                    gs =
                        Group.groupsOf s.state.grouped uuid |> List.map (\i -> ( Uuid.toString i, i )) |> Dict.fromList

                    hadMenu =
                        Config.onlyMenu s.state.configs
                            |> Dict.get (Config.compare (MenuDisplay { what = HType.ProcessType, uuid = uuid, isMenu = False }))
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
                in
                ( { adding
                    | type_ = realType
                    , uuid = uuid
                    , eventTypes = eventTypes
                    , identifiers = getIdentifiers s.state hereType uuid realType False
                    , values = getValues s.state.types s.state.valueTypes s.state.values hereType uuid realType False
                    , gsubmodel = editgroups
                    , hadMenu = hadMenu
                    , isMenu = hadMenu
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

        InputIdentifier i ->
            ( { model | identifiers = Dict.insert (Identifier.compare i) i model.identifiers }
            , Effect.none
            )

        InputValue v ->
            ( { model | values = Dict.insert (Value.compare v) v model.values }
            , Effect.none
            )

        InputEventTypes uuids ->
            ( { model | eventTypes = uuids |> List.map (\u -> ( Uuid.toString u, u )) |> Dict.fromList }
            , Effect.none
            )

        InputIsMenu isMenu ->
            ( { model | isMenu = isMenu }, Effect.none )

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
                            (Payload.AddedProcessType t
                                :: List.map Payload.AddedIdentifier (Dict.values model.identifiers)
                                ++ List.map Payload.AddedValue (Dict.values model.values)
                                ++ List.map (\uuid -> Payload.Grouped (Link hereType t.uuid uuid)) (Dict.values <| Group.Input.added model.gsubmodel)
                                ++ List.map (\uuid -> Payload.Ungrouped (Link hereType t.uuid uuid)) (Dict.values <| Group.Input.removed model.gsubmodel)
                                ++ (if model.hadMenu == model.isMenu then
                                        []

                                    else
                                        [ Payload.Configured <| MenuDisplay { what = HType.ProcessType, uuid = t.uuid, isMenu = model.isMenu } ]
                                   )
                            )
                        , redirect s.navkey (Route.Entity Route.ProcessType (Route.View { uuid = Uuid.toString model.uuid, type_ = Nothing })) |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))


view : Model -> View Msg
view model =
    { title = "Adding a Process Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


checkStep : Model -> Result String ()
checkStep model =
    case model.step of
        _ ->
            Ok ()


validate : Model -> Result String ProcessType
validate m =
    Ok <| ProcessType HType.ProcessType m.uuid m.type_ m.eventTypes


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepType ->
                    flatSelect s.state
                        { what = hereType
                        , muuid = model.type_
                        , onInput = InputType
                        , title = "Parent Type:"
                        , explain = "Optional parent type for the new Process Type (it can be hierarchical)"
                        , empty = "(There are no Process Types yet to choose from)"
                        , additional = Nothing
                        }
                        (s.state.processTypes |> Dict.map (\_ a -> a.uuid))

                Step.Step StepGroups ->
                    Element.map GroupMsg <| inputGroups { type_ = hereType, mpuuid = model.type_ } s.state model.gsubmodel

                Step.Step StepOptions ->
                    column [ alignTop, width <| minimum 200 fill, spacing 10 ]
                        [ h3 "Options:"
                        , row [ Font.size size.text.main ]
                            [ Input.checkbox
                                []
                                { onChange = InputIsMenu
                                , icon = Input.defaultCheckbox
                                , checked = model.isMenu
                                , label = Input.labelRight [] <| text "Display as a menu item"
                                }
                            ]
                        ]

                Step.Step StepIdentifiers ->
                    inputIdentifiers { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputIdentifier } model.identifiers

                Step.Step StepValues ->
                    inputValues { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputValue, context = ( hereType, model.uuid ) } s.state model.values

                Step.Step StepEventTypes ->
                    multiSelect
                        model
                        { inputMsg = InputEventTypes
                        , selection = .eventTypes >> Dict.values
                        , title = "Event Types: "
                        , description = "Select the Event Types that can occur in the processes of this type"
                        , toString = displayZone s.state SmallcardZone (Type.HType HType.EventType)
                        , toDesc = always ""
                        , empty = "There are no Event Types yet to choose from"
                        , height = 100
                        , input = \_ _ _ -> none
                        }
                        (List.map .uuid <| Dict.values s.state.eventTypes)
    in
    floatingContainer s
        (Just <| Button Step.Cancel)
        "Adding a ProcessType"
        (List.map (Element.map Button) (buttons model (checkStep model)))
        [ step
        ]
