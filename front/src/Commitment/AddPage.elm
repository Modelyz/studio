module Commitment.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Agent.Agent exposing (Agent)
import Commitment.Commitment exposing (Commitment)
import CommitmentType.CommitmentType exposing (CommitmentType)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable
import Group.Input exposing (inputGroups)
import Hierarchy.Hierarchic as H
import Ident.Identifiable exposing (withIdentifiers)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType exposing (initIdentifiers)
import Ident.Input exposing (inputIdentifiers)
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended as Random exposing (Seed)
import Route exposing (Route, redirectParent)
import Scope.Scope exposing (Scope(..))
import Shared
import Spa.Page
import Time exposing (millisToPosix)
import Type
import Typed.Type as TType
import Typed.Typed as T
import Value.Rational exposing (Rational(..))
import View exposing (..)
import View.Smallcard exposing (hClickableCard, hViewHalfCard)
import View.Step as Step exposing (Step(..), buttons, isLast)
import View.Style exposing (..)


type alias Flags =
    { route : Route, uuid : Maybe Uuid }


type alias Model =
    { route : Route
    , uuid : Uuid
    , seed : Seed
    , flatselect : Maybe CommitmentType
    , provider : Maybe Agent
    , receiver : Maybe Agent
    , qty : Rational
    , identifiers : Dict String Identifier
    , oldGroups : Dict String Group
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
    = InputType (Maybe CommitmentType)
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
        Route.CommitmentAdd ->
            Just { route = route, uuid = Nothing }

        Route.CommitmentEdit uuid ->
            Just { route = route, uuid = Uuid.fromString uuid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        ( newUuid, newSeed ) =
            Random.step Uuid.generator s.currentSeed
    in
    ( f.uuid
        |> Maybe.andThen (T.find s.state.commitments)
        |> Maybe.map
            (\a ->
                let
                    oldGroups =
                        s.state.grouped
                            |> Dict.filter (\_ v -> a.uuid == Groupable.uuid v.groupable)
                            |> Dict.foldl (\_ v d -> Dict.insert (Group.compare v.group) v.group d) Dict.empty
                in
                { route = f.route
                , flatselect = H.find s.state.commitmentTypes a.type_
                , provider = Nothing
                , receiver = Nothing
                , qty = Rational 0 1
                , uuid = a.uuid
                , seed = newSeed
                , identifiers =
                    initIdentifiers s.state.commitments s.state.commitmentTypes s.state.identifierTypes (Type.TType TType.Commitment) Nothing a.uuid
                        |> Dict.union (Identifier.fromUuid a.uuid s.state.identifiers)
                , oldGroups = oldGroups
                , groups = oldGroups
                , warning = ""
                , step = Step.Step StepType
                , steps = [ Step.Step StepType, Step.Step StepIdentifiers, Step.Step StepGroups ]
                }
            )
        |> Maybe.withDefault
            { route = f.route
            , flatselect = Nothing
            , provider = Nothing
            , receiver = Nothing
            , qty = Rational 0 1
            , uuid = newUuid
            , seed = newSeed
            , identifiers = initIdentifiers s.state.commitments s.state.commitmentTypes s.state.identifierTypes (Type.TType TType.Commitment) Nothing newUuid
            , oldGroups = Dict.empty
            , groups = Dict.empty
            , warning = ""
            , step = Step.Step StepType
            , steps = [ Step.Step StepType, Step.Step StepIdentifiers, Step.Step StepGroups ]
            }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputType mat ->
            ( { model
                | flatselect = mat
                , identifiers = initIdentifiers s.state.commitments s.state.commitmentTypes s.state.identifierTypes (Type.TType TType.Commitment) mat model.uuid
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
                Ok a ->
                    let
                        addedGroups =
                            Dict.diff model.groups model.oldGroups

                        removedGroups =
                            Dict.diff model.oldGroups model.groups
                    in
                    ( model
                    , Effect.batch
                        [ Shared.dispatchMany s
                            (Message.AddedCommitment a
                                :: List.map Message.IdentifierAdded (Dict.values model.identifiers)
                                ++ List.map (\g -> Message.Grouped (Groupable.Cm a) g) (Dict.values addedGroups)
                                ++ List.map (\g -> Message.Ungrouped (Groupable.Cm a) g) (Dict.values removedGroups)
                            )
                        , redirectParent s.navkey model.route |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding a Commitment Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


checkStep : Model -> Result String ()
checkStep model =
    case model.step of
        Step StepType ->
            Maybe.map (\_ -> Ok ()) model.flatselect |> Maybe.withDefault (Err "You must select a Commitment Type")

        Step StepIdentifiers ->
            Ok ()

        Step StepGroups ->
            Ok ()


validate : Model -> Result String Commitment
validate m =
    Result.map3 (\ct provider receiver -> Commitment (Type.TType TType.Commitment) m.uuid ct.uuid (millisToPosix 0) Dict.empty Dict.empty Dict.empty Dict.empty m.qty provider.uuid receiver.uuid)
        (checkMaybe m.flatselect "You must select a Commitment Type")
        (checkMaybe m.provider "You must select a provider")
        (checkMaybe m.receiver "You must select a receiver")


buttonValidate : Model -> Result String field -> Element Msg
buttonValidate m result =
    -- TODO try to suppress using at View.Step.nextMsg
    case result of
        Ok _ ->
            if isLast m.step m.steps then
                button.primary Added "Validate and finish"

            else
                none

        Err _ ->
            none


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepType ->
                    let
                        allHwithIdentifiers =
                            s.state.commitmentTypes |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })
                    in
                    column [ alignTop, spacing 10, width <| minimum 200 fill ]
                        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
                            [ h2 "Type"
                            , model.flatselect
                                |> Maybe.map (withIdentifiers s.state.commitments s.state.commitmentTypes s.state.identifierTypes s.state.identifiers)
                                |> Maybe.map (hViewHalfCard (InputType Nothing) s.state.commitments allHwithIdentifiers s.state.configs)
                                |> Maybe.withDefault (el [ padding 5, Font.color color.text.disabled ] (text "Empty"))
                            ]
                        , h2 "Choose the type of the new Commitment:"
                        , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ]
                            (allHwithIdentifiers
                                |> Dict.values
                                |> List.map (hClickableCard InputType s.state.commitments allHwithIdentifiers s.state.configs)
                                |> withDefaultContent (p "(There are no Commitment Types yet)")
                            )
                        ]

                Step.Step StepGroups ->
                    inputGroups { onInput = InputGroups } s model

                Step.Step StepIdentifiers ->
                    let
                        scope =
                            model.flatselect |> Maybe.map (\h -> HasUserType (Type.TType TType.Commitment) h.uuid) |> Maybe.withDefault (HasType (Type.TType TType.Commitment))
                    in
                    inputIdentifiers { onEnter = Step.nextMsg model Button Step.NextPage Added, onInput = InputIdentifier } model scope
    in
    floatingContainer s
        "Adding a Commitment"
        (List.map (Element.map Button) (buttons model (checkStep model))
            ++ [ buttonValidate model (checkStep model) ]
        )
        [ step
        ]
