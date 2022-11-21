module Event.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Event.Event exposing (Event)
import EventType.EventType exposing (EventType)
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
import Shared
import Spa.Page
import Time exposing (millisToPosix)
import Type exposing (Type)
import Typed.Type as TType
import Value.Input exposing (inputValues)
import Value.Valuable exposing (getValues)
import Value.Value as Value exposing (Value)
import View exposing (..)
import View.FlatSelect exposing (flatSelect)
import View.Step as Step exposing (Step(..), buttons)


constructor =
    Event


typedConstructor : TType.Type
typedConstructor =
    TType.Event


hereType : Type.Type
hereType =
    Type.TType TType.Event


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
        Route.Entity Route.Event Route.Add ->
            Just { route = route, uuid = Nothing }

        Route.Entity Route.Event (Route.Edit uuid) ->
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
            , uuid = newUuid
            , seed = newSeed
            , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType newUuid
            , values = getValues s.state.types s.state.valueTypes s.state.values hereType newUuid
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
                in
                { adding
                    | type_ = Dict.get (Uuid.toString uuid) s.state.types |> Maybe.andThen (\( _, _, x ) -> x)
                    , uuid = uuid
                    , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType newUuid
                    , values = getValues s.state.types s.state.valueTypes s.state.values hereType newUuid
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
                , identifiers = getIdentifiers s.state.types s.state.identifierTypes s.state.identifiers hereType model.uuid
                , values = getValues s.state.types s.state.valueTypes s.state.values hereType model.uuid
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
                            (Message.AddedEvent t
                                :: List.map Message.AddedIdentifier (Dict.values model.identifiers)
                                ++ List.map Message.AddedValue (Dict.values model.values)
                                ++ List.map (\uuid -> Message.Grouped (Link hereType t.uuid uuid)) (Dict.values addedGroups)
                                ++ List.map (\uuid -> Message.Ungrouped (Link hereType t.uuid uuid)) (Dict.values removedGroups)
                            )
                        , redirect s.navkey (Route.Entity Route.Event (Route.View (Uuid.toString model.uuid))) |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding an Event"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


checkStep : Model -> Result String ()
checkStep model =
    case model.step of
        Step StepType ->
            Maybe.map (\_ -> Ok ()) model.type_ |> Maybe.withDefault (Err "You must select an Event Type")

        Step StepIdentifiers ->
            Ok ()

        Step StepValues ->
            Ok ()

        Step StepGroups ->
            Ok ()


validate : Model -> Result String Event
validate m =
    case m.type_ of
        Just uuid ->
            -- TODO check that TType thing is useful
            Ok <| constructor typedConstructor m.uuid uuid (millisToPosix 0)

        Nothing ->
            Err "You must select an Event Type"


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepType ->
                    flatSelect s
                        { what = Type.TType TType.Event
                        , muuid = Just model.uuid
                        , onInput = InputType
                        , title = "Type:"
                        , explain = "Choose the type of the new Event:"
                        , empty = "(There are no Event Types yet to choose from)"
                        }
                        (s.state.agentTypes |> Dict.map (\_ a -> a.uuid))

                Step.Step StepGroups ->
                    inputGroups { onInput = InputGroups } s model.groups

                Step.Step StepIdentifiers ->
                    inputIdentifiers { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputIdentifier } model

                Step.Step StepValues ->
                    inputValues { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputValue } s model
    in
    floatingContainer s
        (Just <| Button Step.Cancel)
        "Adding an Event"
        (List.map (Element.map Button) (buttons model (checkStep model)))
        [ step
        ]
