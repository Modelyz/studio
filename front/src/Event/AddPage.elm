module Event.AddPage exposing (..)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Event.Event as Event exposing (Event)
import EventType.EventType as EventType exposing (EventType)
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable exposing (Groupable)
import Group.Input exposing (inputGroups)
import Hierarchy.View exposing (toDesc)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType exposing (initIdentifiers)
import Ident.Input exposing (inputIdentifiers)
import Item.Item as Item
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended as Random exposing (Seed, initialSeed)
import Route exposing (Route, redirectParent)
import Scope.Scope as Scope exposing (Scope(..))
import Shared
import Spa.Page
import Time exposing (millisToPosix)
import Type
import Typed.Type as TType
import View exposing (..)
import View.Step as Step exposing (Step(..), buttons, isLast)
import View.Style exposing (..)


type alias Flags =
    { route : Route }


type alias Model =
    { route : Route
    , uuid : Uuid
    , seed : Seed
    , flatselect : Maybe EventType
    , identifiers : Dict String Identifier
    , groups : Dict String Group
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepType
    | StepIdentifiers
    | StepGroups


type Msg
    = InputType (Maybe EventType)
    | InputIdentifier Identifier
    | InputGroups (Dict String Group)
    | Added
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
        Route.EventAdd ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        ( newUuid, newSeed ) =
            Random.step Uuid.generator s.currentSeed
    in
    ( { route = f.route
      , flatselect = Nothing
      , uuid = newUuid
      , seed = newSeed
      , identifiers = initIdentifiers s.state.events s.state.eventTypes s.state.identifierTypes (Type.TType TType.Event) Nothing newUuid
      , groups = Dict.empty
      , warning = ""
      , step = Step.Step StepType
      , steps = [ Step.Step StepType, Step.Step StepIdentifiers, Step.Step StepGroups ]
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    let
        ( newUuid, newSeed ) =
            Random.step Uuid.generator model.seed
    in
    case msg of
        InputType met ->
            ( { model
                | flatselect = met
                , identifiers = initIdentifiers s.state.events s.state.eventTypes s.state.identifierTypes (Type.TType TType.Event) met newUuid
                , uuid = newUuid
                , seed = newSeed
              }
            , Effect.none
            )

        InputIdentifier i ->
            ( { model | identifiers = Dict.insert (Identifier.compare i) i model.identifiers }, Effect.none )

        InputGroups gs ->
            ( { model | groups = gs }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))

        Added ->
            case validate model of
                Ok e ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatchMany s
                            (Message.AddedEvent e
                                :: List.map Message.IdentifierAdded (Dict.values model.identifiers)
                                ++ List.map (\g -> Message.Grouped (Groupable.E e) g) (Dict.values model.groups)
                            )
                        , redirectParent s.navkey model.route |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )


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
            checkNothing model.flatselect "Please choose a type"
                |> Result.map (\_ -> ())

        Step StepIdentifiers ->
            Ok ()

        Step StepGroups ->
            Ok ()


validate : Model -> Result String Event
validate m =
    case m.flatselect of
        Just rt ->
            -- TODO when?
            Ok <| Event (Type.TType TType.Event) m.uuid rt.uuid (millisToPosix 0) Dict.empty

        Nothing ->
            Err "You must select an Event Type"


buttonValidate : Model -> Result String field -> Element Msg
buttonValidate m result =
    case result of
        Ok _ ->
            if isLast m.step m.steps then
                button.primary Added "Validate and finish"

            else
                none

        Err err ->
            none


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepType ->
                    column [ alignTop, spacing 10, width <| minimum 200 fill ]
                        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
                            [ h2 "Type"
                            , Maybe.map
                                (\rt ->
                                    row [ Background.color color.item.selected ]
                                        [ el [ padding 5 ] (text (Uuid.toString rt.uuid))
                                        , button.secondary (InputType Nothing) "×"
                                        ]
                                )
                                model.flatselect
                                |> Maybe.withDefault
                                    (el
                                        [ padding 5, Font.color color.text.disabled ]
                                        (text "Empty")
                                    )
                            ]
                        , h2 "Choose the type of the new Event"
                        , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ] <|
                            List.map
                                (\rt -> clickableCard (InputType <| Just rt) (text <| Uuid.toString rt.uuid) (toDesc s.state.eventTypes rt))
                                (Dict.values <| s.state.eventTypes)
                        ]

                Step.Step StepGroups ->
                    inputGroups { onInput = InputGroups } s model

                Step.Step StepIdentifiers ->
                    inputIdentifiers { onEnter = Added, onInput = InputIdentifier } model
    in
    floatingContainer s
        "Adding an Event"
        (List.map (Element.map Button) (buttons model (checkStep model))
            ++ [ buttonValidate model (checkStep model) ]
        )
        [ step
        ]
