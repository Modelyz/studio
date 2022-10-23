module ContractType.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Contract.Contract exposing (Contract)
import ContractType.ContractType exposing (ContractType)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable
import Group.Input exposing (inputGroups)
import Hierarchy.Hierarchic as H
import Hierarchy.Type as HType
import Ident.Identifiable exposing (hWithIdentifiers)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType exposing (initIdentifiers)
import Ident.Input exposing (inputIdentifiers)
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended as Random exposing (Seed)
import Route exposing (Route, redirect, redirectToView)
import Scope.Scope exposing (Scope(..))
import Shared
import Spa.Page
import State exposing (State)
import Type
import Typed.Type as TType
import Value.Input exposing (inputValues)
import Value.Value as Value exposing (Value)
import Value.ValueType exposing (initValues)
import View exposing (..)
import View.Smallcard exposing (hClickableCard, hViewHalfCard)
import View.Step as Step exposing (Step(..), buttons)
import View.Style exposing (..)


hereType : Type.Type
hereType =
    Type.HType HType.ContractType


type alias HierarchicType =
    ContractType


tType =
    ContractType


mkMessage : HierarchicType -> Message.Payload
mkMessage =
    Message.AddedContractType


allT : Shared.Model -> Dict String Contract
allT =
    .state >> .contracts


allH : Shared.Model -> Dict String ContractType
allH =
    .state >> .contractTypes


type alias Flags =
    { route : Route, uuid : Maybe Uuid }


type alias Model =
    { route : Route
    , uuid : Uuid
    , seed : Seed
    , flatselect : Maybe HierarchicType
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , oldGroups : Dict String Group
    , groups : Dict String Group
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
    = InputType (Maybe HierarchicType)
    | InputIdentifier Identifier
    | InputGroups (Dict String Group)
    | InputValue Value
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
        Route.ContractTypeAdd ->
            Just { route = route, uuid = Nothing }

        Route.ContractTypeEdit uuid ->
            Just { route = route, uuid = Uuid.fromString uuid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        ( newUuid, newSeed ) =
            Random.step Uuid.generator s.currentSeed

        adding =
            { route = f.route
            , flatselect = Nothing
            , uuid = newUuid
            , seed = newSeed
            , identifiers = initIdentifiers (allT s) (allH s) s.state.identifierTypes hereType Nothing newUuid
            , values = initValues (allT s) (allH s) s.state.valueTypes hereType Nothing newUuid
            , oldGroups = Dict.empty
            , groups = Dict.empty
            , warning = ""
            , step = Step.Step StepType
            , steps = [ Step.Step StepType, Step.Step StepIdentifiers, Step.Step StepValues, Step.Step StepGroups ]
            }
    in
    ( f.uuid
        |> Maybe.andThen (H.find (allH s))
        |> Maybe.map
            (\h ->
                let
                    oldGroups =
                        s.state.grouped
                            |> Dict.filter (\_ v -> h.uuid == Groupable.uuid v.groupable)
                            |> Dict.foldl (\_ v d -> Dict.insert (Group.compare v.group) v.group d) Dict.empty

                    mp =
                        h.parent |> Maybe.andThen (H.find (allH s))
                in
                { adding
                    | flatselect = mp
                    , uuid = h.uuid
                    , identifiers =
                        initIdentifiers (allT s) (allH s) s.state.identifierTypes hereType mp h.uuid
                            |> Dict.union (Identifier.fromUuid h.uuid s.state.identifiers)
                    , values =
                        initValues (allT s) (allH s) s.state.valueTypes hereType mp h.uuid
                            |> Dict.union (Dict.filter (\_ i -> h.uuid == i.for) s.state.values)
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
                | flatselect = mh
                , identifiers = initIdentifiers (allT s) (allH s) s.state.identifierTypes hereType mh model.uuid
                , values = initValues (allT s) (allH s) s.state.valueTypes hereType mh model.uuid
              }
            , Effect.none
            )

        InputIdentifier i ->
            ( { model | identifiers = Dict.insert (Identifier.compare i) i model.identifiers }, Effect.none )

        InputValue v ->
            ( { model | values = Dict.insert (Value.compare v) v model.values }, Effect.none )

        InputGroups gs ->
            ( { model | groups = gs }, Effect.none )

        Button Step.Added ->
            case validate model of
                Ok h ->
                    let
                        addedGroups =
                            Dict.diff model.groups model.oldGroups

                        removedGroups =
                            Dict.diff model.oldGroups model.groups
                    in
                    ( model
                    , Effect.batch
                        [ Shared.dispatchMany s
                            (mkMessage h
                                :: List.map Message.IdentifierAdded (Dict.values model.identifiers)
                                ++ List.map (\g -> Message.Grouped (Groupable.RT h) g) (Dict.values addedGroups)
                                ++ List.map (\g -> Message.Ungrouped (Groupable.RT h) g) (Dict.values removedGroups)
                            )
                        , redirect s.navkey (Route.ContractTypeView (Uuid.toString model.uuid)) |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding a Contract Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


checkStep : Model -> Result String ()
checkStep model =
    case model.step of
        Step StepType ->
            Ok ()

        Step StepIdentifiers ->
            Ok ()

        Step StepValues ->
            Ok ()

        Step StepGroups ->
            Ok ()


validate : Model -> Result String HierarchicType
validate m =
    Ok <| tType hereType m.uuid (Maybe.map .uuid m.flatselect) Dict.empty Dict.empty Dict.empty Dict.empty


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepType ->
                    let
                        allHwithIdentifiers =
                            allH s |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })
                    in
                    column [ alignTop, spacing 10, width <| minimum 200 fill ]
                        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
                            [ h2 "Type"
                            , model.flatselect
                                |> Maybe.map (hWithIdentifiers (allT s) (allH s) s.state.identifierTypes s.state.identifiers)
                                |> Maybe.map (hViewHalfCard (InputType Nothing) (allT s) allHwithIdentifiers s.state.configs)
                                |> Maybe.withDefault (el [ padding 5, Font.color color.text.disabled ] (text "Empty"))
                            ]
                        , h2 "Optional parent type for the new Contract Type (it can be hierarchical)"
                        , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ]
                            (allHwithIdentifiers
                                |> Dict.values
                                |> List.map (hClickableCard InputType (allT s) allHwithIdentifiers s.state.configs)
                                |> withDefaultContent (p "(There are no Contract Types yet)")
                            )
                        ]

                Step.Step StepGroups ->
                    inputGroups { onInput = InputGroups } s model

                Step.Step StepIdentifiers ->
                    let
                        scope =
                            model.flatselect |> Maybe.map (\h -> HasUserType hereType h.uuid) |> Maybe.withDefault (HasType hereType)
                    in
                    inputIdentifiers { onEnter = Step.nextMsg model Button Step.NextPage Step.Added, onInput = InputIdentifier } model scope

                Step.Step StepValues ->
                    let
                        scope =
                            model.flatselect |> Maybe.map (\h -> HasUserType hereType h.uuid) |> Maybe.withDefault (HasType hereType)
                    in
                    inputValues
                        { onEnter = Step.nextMsg model Button Step.NextPage Step.Added
                        , onInput = InputValue
                        }
                        s
                        model
                        scope
    in
    floatingContainer s (Just <| Button Step.Cancel)  
        "Adding a ContractType"
        (List.map (Element.map Button) (buttons model (checkStep model)))
        [ step
        ]
