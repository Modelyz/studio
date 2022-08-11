module Group.AddPage exposing (..)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable exposing (Groupable)
import Group.Input exposing (inputGroups)
import GroupType.GroupType as GroupType exposing (GroupType)
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
    , flatselect : Maybe GroupType
    , identifiers : DictSet String Identifier
    , groups : DictSet String Group
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepType
    | StepIdentifiers
    | StepGroups


type Msg
    = InputType (Maybe GroupType)
    | InputIdentifier Identifier
    | InputGroups (DictSet String Group)
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
        Route.GroupAdd ->
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
      , identifiers = initIdentifiers s.state.groups s.state.groupTypes s.state.identifierTypes (Type.TType TType.Group) Nothing newUuid
      , groups = Set.empty Group.compare
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
        InputType mgt ->
            ( { model
                | flatselect = mgt
                , identifiers = initIdentifiers s.state.groups s.state.groupTypes s.state.identifierTypes (Type.TType TType.Group) mgt newUuid
                , uuid = newUuid
                , seed = newSeed
              }
            , Effect.none
            )

        InputIdentifier i ->
            ( { model | identifiers = Set.insert i model.identifiers }, Effect.none )

        InputGroups gs ->
            ( { model | groups = gs }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))

        Added ->
            case validate model of
                Ok g ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatchMany s
                            (Message.DefinedGroup g
                                :: List.map Message.IdentifierAdded (Set.toList model.identifiers)
                                ++ List.map (\ga -> Message.Grouped (Groupable.G ga) g) (Set.toList model.groups)
                            )
                        , redirectParent s.navkey model.route |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Groups"
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


validate : Model -> Result String Group
validate m =
    case m.flatselect of
        Just rt ->
            -- TODO input Scope
            Ok <| Group (Type.TType TType.Group) m.uuid rt.uuid Empty

        Nothing ->
            Err "You must select an Group Type"


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
                        , h2 "Choose the type of the new Group (it can be hierarchical"
                        , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ] <|
                            List.map
                                (\rt -> clickableCard (InputType <| Just rt) (text <| Uuid.toString rt.uuid) (toDesc s.state.groupTypes rt))
                                (Set.toList <| s.state.groupTypes)
                        ]

                Step.Step StepGroups ->
                    inputGroups { onInput = InputGroups } s model

                Step.Step StepIdentifiers ->
                    inputIdentifiers { onEnter = Added, onInput = InputIdentifier } model
    in
    floatingContainer s
        "Adding an Group"
        (List.map (Element.map Button) (buttons model (checkStep model))
            ++ [ buttonValidate model (checkStep model) ]
        )
        [ step
        ]
