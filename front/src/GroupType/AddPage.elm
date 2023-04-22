module GroupType.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Configuration as Config exposing (Configuration(..))
import Configuration.Zone exposing (Zone(..))
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Group.Group as Group
import Group.Input exposing (inputGroups)
import Group.Link exposing (Link)
import GroupType.GroupType exposing (GroupType)
import Hierarchy.Type as HType
import Ident.Identifiable exposing (getIdentifiers)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.Input exposing (inputIdentifiers)
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended as Random exposing (Seed)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import Tree exposing (Type(..))
import Tree.View exposing (inputTreeType)
import Type
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
    Type.HType HType.GroupType


type alias Flags =
    { route : Route, uuid : Maybe Uuid }


type alias Model =
    { route : Route
    , isNew : Bool
    , uuid : Uuid
    , seed : Seed
    , type_ : Maybe Uuid
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , gsubmodel : Group.Input.Model
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    , hadMenu : Bool
    , isMenu : Bool
    , unique : Bool
    , treeType : Tree.Type
    }


type Step
    = StepType
    | StepUnique
    | StepTree
    | StepIdentifiers
    | StepValues
    | StepGroups
    | StepOptions


type Msg
    = InputType (Maybe Uuid)
    | InputIdentifier Identifier
    | InputIsMenu Bool
    | GroupMsg Group.Input.Msg
    | InputValue Value
    | InputUnique Bool
    | InputTreeType Tree.Type
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
        Route.Entity Route.GroupType (Route.Add _ _) ->
            Just { route = route, uuid = Nothing }

        Route.Entity Route.GroupType (Route.Edit uuid _) ->
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

        ( initgroups, initcmd ) =
            Group.Input.init s Dict.empty

        adding =
            { route = f.route
            , isNew = isNew
            , type_ = Nothing
            , uuid = newUuid
            , seed = newSeed
            , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType newUuid Nothing True
            , values = getValues s.state.types s.state.valueTypes s.state.values hereType newUuid Nothing True
            , gsubmodel = initgroups
            , isMenu = True
            , hadMenu = True
            , warning = ""
            , step = Step.Step StepType
            , steps =
                [ Step.Step StepType
                , Step.Step StepIdentifiers
                , Step.Step StepUnique
                , Step.Step StepTree
                , Step.Step StepValues
                , Step.Step StepOptions
                , Step.Step StepGroups
                ]
            , unique = False
            , treeType = Flat
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
                            |> Dict.get (Config.compare (MenuDisplay HType.GroupType uuid False))
                            |> Maybe.map
                                (\config ->
                                    case config of
                                        MenuDisplay _ _ isMenu ->
                                            isMenu

                                        _ ->
                                            True
                                )
                            |> Maybe.withDefault True

                    ( editgroups, editcmd ) =
                        Group.Input.init s gs

                    gt =
                        Dict.get (Uuid.toString uuid) s.state.groupTypes
                in
                ( { adding
                    | type_ = realType
                    , uuid = uuid
                    , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType uuid realType False
                    , values = getValues s.state.types s.state.valueTypes s.state.values hereType uuid realType False
                    , gsubmodel = editgroups
                    , hadMenu = hadMenu
                    , isMenu = hadMenu
                    , unique = Maybe.map .unique gt |> Maybe.withDefault True
                    , treeType = Maybe.map .treeType gt |> Maybe.withDefault Flat
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

        InputIdentifier i ->
            ( { model | identifiers = Dict.insert (Identifier.compare i) i model.identifiers }, Effect.none )

        InputValue v ->
            ( { model | values = Dict.insert (Value.compare v) v model.values }, Effect.none )

        InputUnique b ->
            ( { model | unique = b }, Effect.none )

        InputTreeType t ->
            ( { model | treeType = t }, Effect.none )

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
                            (Message.AddedGroupType t
                                :: List.map Message.AddedIdentifier (Dict.values model.identifiers)
                                ++ List.map Message.AddedValue (Dict.values model.values)
                                ++ List.map (\uuid -> Message.Grouped (Link hereType t.uuid uuid)) (Dict.values <| Group.Input.added model.gsubmodel)
                                ++ List.map (\uuid -> Message.Ungrouped (Link hereType t.uuid uuid)) (Dict.values <| Group.Input.removed model.gsubmodel)
                                ++ (if model.hadMenu == model.isMenu then
                                        []

                                    else
                                        [ Message.Configured <| MenuDisplay HType.GroupType t.uuid model.isMenu ]
                                   )
                            )
                        , redirect s.navkey (Route.Entity Route.GroupType (Route.View (Uuid.toString model.uuid) Nothing)) |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))


view : Shared.Model -> Model -> View Msg
view _ model =
    { title = "Adding a Group Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


checkStep : Model -> Result String ()
checkStep model =
    case model.step of
        _ ->
            Ok ()


validate : Model -> Result String GroupType
validate m =
    Ok <| GroupType HType.GroupType m.uuid m.type_ m.unique m.treeType


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepType ->
                    flatSelect s
                        { what = hereType
                        , muuid = model.type_
                        , onInput = InputType
                        , title = "Parent Type:"
                        , explain = "Optional parent type for the new Group Type (it can be hierarchical)"
                        , empty = "(There are no Group Types yet to choose from)"
                        }
                        (s.state.groupTypes |> Dict.map (\_ a -> a.uuid))

                Step.Step StepGroups ->
                    Element.map GroupMsg <| inputGroups { type_ = hereType, mpuuid = model.type_ } s model.gsubmodel

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
                    inputValues { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputValue, context = ( hereType, model.uuid ) } s model.values

                Step.Step StepUnique ->
                    wrappedRow [ spacing 20 ] [ text "An entity can only be in one group of this type: ", switch InputUnique model.unique "False" "True" ]

                Step.Step StepTree ->
                    wrappedRow [ spacing 20 ] [ text "Groups of this type are:", inputTreeType InputTreeType model.treeType ]
    in
    floatingContainer s
        (Just <| Button Step.Cancel)
        "Adding an GroupType"
        (List.map (Element.map Button) (buttons model (checkStep model)))
        [ step
        ]
