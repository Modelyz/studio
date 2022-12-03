module Resource.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Group.Input exposing (inputGroups)
import Group.Link exposing (Link)
import Hierarchy.Type as HType
import Ident.Identifiable exposing (getIdentifiers)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.Input exposing (inputIdentifiers)
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended as Random exposing (Seed)
import Resource.Resource exposing (Resource)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import Type
import Typed.Type as TType
import Value.Input exposing (inputValues)
import Value.Valuable exposing (getValues)
import Value.Value as Value exposing (Value)
import View exposing (..)
import View.FlatSelect exposing (flatSelect)
import View.Step as Step exposing (Step(..), buttons)


constructor =
    Resource


typedConstructor : TType.Type
typedConstructor =
    TType.Resource


hereType : Type.Type
hereType =
    Type.TType TType.Resource


type alias Flags =
    { route : Route, uuid : Maybe Uuid, tuuid : Maybe String }


type alias Model =
    { route : Route
    , isNew : Bool
    , uuid : Uuid
    , seed : Seed
    , type_ : Maybe Uuid
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
    | StepValues
    | StepGroups


type Msg
    = InputType (Maybe Uuid)
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
        Route.Entity Route.Resource (Route.Add tuuid) ->
            Just { route = route, uuid = Nothing, tuuid = tuuid }

        Route.Entity Route.Resource (Route.Edit uuid tuuid) ->
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
            , uuid = newUuid
            , seed = newSeed
            , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType newUuid type_ True
            , values = getValues s.state.types s.state.valueTypes s.state.values hereType newUuid type_ True
            , oldGroups = Dict.empty
            , groups = Dict.empty
            , warning = ""
            , step = Step.Step StepType
            , steps = [ Step.Step StepType, Step.Step StepIdentifiers, Step.Step StepValues, Step.Step StepGroups ]
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
                        Dict.get (Uuid.toString uuid) s.state.types |> Maybe.andThen (\( _, _, x ) -> x)
                in
                { adding
                    | type_ = type_
                    , uuid = uuid
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
                            (Message.AddedResource t
                                :: List.map Message.AddedIdentifier (Dict.values model.identifiers)
                                ++ List.map Message.AddedValue (Dict.values model.values)
                                ++ List.map (\uuid -> Message.Grouped (Link hereType t.uuid uuid)) (Dict.values addedGroups)
                                ++ List.map (\uuid -> Message.Ungrouped (Link hereType t.uuid uuid)) (Dict.values removedGroups)
                            )
                        , redirect s.navkey (Route.Entity Route.Resource (Route.View (Uuid.toString model.uuid) (Maybe.map Uuid.toString model.type_))) |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding a Resource"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


checkStep : Model -> Result String ()
checkStep model =
    case model.step of
        Step StepType ->
            Maybe.map (\_ -> Ok ()) model.type_ |> Maybe.withDefault (Err "You must select a Resource Type")

        Step StepIdentifiers ->
            Ok ()

        Step StepValues ->
            Ok ()

        Step StepGroups ->
            Ok ()


validate : Model -> Result String Resource
validate m =
    case m.type_ of
        Just uuid ->
            -- TODO check that TType thing is useful
            Ok <| constructor typedConstructor m.uuid uuid

        Nothing ->
            Err "You must select a Resource Type"


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepType ->
                    flatSelect s
                        { what = Type.HType HType.ResourceType
                        , muuid = model.type_
                        , onInput = InputType
                        , title = "Type:"
                        , explain = "Choose the type of the new Resource:"
                        , empty = "(There are no Resource Types yet to choose from)"
                        }
                        (s.state.resourceTypes |> Dict.map (\_ a -> a.uuid))

                Step.Step StepGroups ->
                    inputGroups { onInput = InputGroups, type_ = hereType, mpuuid = model.type_ } s model.groups

                Step.Step StepIdentifiers ->
                    inputIdentifiers { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputIdentifier } model.identifiers

                Step.Step StepValues ->
                    inputValues { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputValue, context = ( hereType, model.uuid ) } s model.values
    in
    floatingContainer s
        (Just <| Button Step.Cancel)
        "Adding a Resource"
        (List.map (Element.map Button) (buttons model (checkStep model)))
        [ step
        ]
