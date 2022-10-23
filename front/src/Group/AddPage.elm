module Group.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Group.Group exposing (Group)
import GroupType.GroupType exposing (GroupType)
import Hierarchy.Hierarchic as H
import Ident.Identifiable exposing (hWithIdentifiers)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType exposing (initIdentifiers)
import Ident.Input exposing (inputIdentifiers)
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended as Random exposing (Seed)
import Route exposing (Route, redirect)
import Scope.Scope exposing (Scope(..))
import Shared
import Spa.Page
import Type
import Typed.Type as TType
import Typed.Typed as T
import Value.Input exposing (inputValues)
import Value.Value as Value exposing (Value)
import Value.ValueType exposing (initValues)
import View exposing (..)
import View.Smallcard exposing (hClickableCard, hViewHalfCard)
import View.Step as Step exposing (Step(..), buttons)
import View.Style exposing (..)


type alias TypedType =
    Group


type alias HierarchicType =
    GroupType


tType =
    Group


hereType : Type.Type
hereType =
    Type.TType TType.Group


mkMessage : TypedType -> Message.Payload
mkMessage =
    Message.DefinedGroup


allT : Shared.Model -> Dict String Group
allT =
    .state >> .groups


allH : Shared.Model -> Dict String GroupType
allH =
    .state >> .groupTypes


type alias Flags =
    { route : Route, uuid : Maybe Uuid }


type alias Model =
    { route : Route
    , uuid : Uuid
    , seed : Seed
    , flatselect : Maybe HierarchicType
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepType
    | StepIdentifiers
    | StepValues


type Msg
    = InputType (Maybe HierarchicType)
    | InputIdentifier Identifier
    | InputValue Value
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
            Just { route = route, uuid = Nothing }

        Route.GroupEdit uuid ->
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
            , warning = ""
            , step = Step.Step StepType
            , steps = [ Step.Step StepType, Step.Step StepIdentifiers, Step.Step StepValues ]
            }
    in
    ( f.uuid
        |> Maybe.andThen (T.find (allT s))
        |> Maybe.map
            (\t ->
                let
                    parent =
                        H.find (allH s) t.type_
                in
                { adding
                    | flatselect = parent
                    , uuid = t.uuid
                    , identifiers =
                        initIdentifiers (allT s) (allH s) s.state.identifierTypes hereType parent t.uuid
                            |> Dict.union (Identifier.fromUuid t.uuid s.state.identifiers)
                    , values =
                        initValues (allT s) (allH s) s.state.valueTypes hereType parent t.uuid
                            |> Dict.union (Dict.filter (\_ i -> t.uuid == i.for) s.state.values)
                    , warning = ""
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

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))

        Added ->
            case validate model of
                Ok t ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatchMany s
                            (mkMessage t
                                :: List.map Message.IdentifierAdded (Dict.values model.identifiers)
                                ++ List.map Message.ValueAdded (Dict.values model.values)
                            )
                        , redirect s.navkey (Route.GroupView (Uuid.toString model.uuid)) |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding a Group Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


checkStep : Model -> Result String ()
checkStep model =
    case model.step of
        Step StepType ->
            Maybe.map (\_ -> Ok ()) model.flatselect |> Maybe.withDefault (Err "You must select a Group Type")

        Step StepIdentifiers ->
            Ok ()

        Step StepValues ->
            Ok ()


validate : Model -> Result String TypedType
validate m =
    case m.flatselect of
        Just h ->
            -- TODO check that TType thing is useful
            Ok <| tType hereType m.uuid h.uuid Empty Dict.empty Dict.empty Dict.empty

        Nothing ->
            Err "You must select a Group Type"


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
                                |> Maybe.map (hViewHalfCard (InputType Nothing) (allT s) allHwithIdentifiers s.state.configs)
                                |> Maybe.withDefault (el [ padding 5, Font.color color.text.disabled ] (text "Empty"))
                            ]
                        , h2 "Choose the type of the new Group:"
                        , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ]
                            (allHwithIdentifiers
                                |> Dict.values
                                |> List.map (hClickableCard InputType (allT s) allHwithIdentifiers s.state.configs)
                                |> withDefaultContent (p "(There are no Group Types yet)")
                            )
                        ]

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
        "Adding a Group"
        (List.map (Element.map Button) (buttons model (checkStep model)))
        [ step
        ]
