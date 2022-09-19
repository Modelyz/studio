module Group.AddPage exposing (..)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable exposing (Groupable)
import Group.Input exposing (inputGroups)
import GroupType.GroupType as GroupType exposing (GroupType)
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Hierarchy.Type as TType
import Hierarchy.View exposing (toDesc)
import Ident.Identifiable exposing (hWithIdentifiers, tWithIdentifiers, withIdentifiers)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType exposing (initIdentifiers)
import Ident.Input exposing (inputIdentifiers)
import Item.Item as Item exposing (Item)
import Json.Decode as Decode
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended as Random exposing (Seed, initialSeed)
import Route exposing (Route, redirectParent)
import Scope.Scope as Scope exposing (Scope(..))
import Shared exposing (flip)
import Spa.Page
import State exposing (State)
import Time exposing (millisToPosix)
import Type exposing (Type)
import Typed.Type as TType
import Typed.Typed as T
import View exposing (..)
import View.Smallcard exposing (hClickableCard, hViewHalfCard, hViewSmallCard)
import View.Step as Step exposing (Step(..), buttons, isLast)
import View.Style exposing (..)


type alias Flags =
    { route : Route, uuid : Maybe Uuid }


type alias Model =
    { route : Route
    , uuid : Uuid
    , seed : Seed
    , flatselect : Maybe GroupType
    , identifiers : Dict String Identifier
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepType
    | StepIdentifiers


type Msg
    = InputType (Maybe GroupType)
    | InputIdentifier Identifier
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
    in
    ( f.uuid
        |> Maybe.andThen (T.find s.state.groups)
        |> Maybe.map
            (\a ->
                let
                    oldGroups =
                        s.state.grouped
                            |> Dict.filter (\_ v -> a.uuid == Groupable.uuid v.groupable)
                            |> Dict.foldl (\_ v d -> Dict.insert (Group.compare v.group) v.group d) Dict.empty
                in
                { route = f.route
                , flatselect = H.find s.state.groupTypes a.type_
                , uuid = a.uuid
                , seed = newSeed
                , identifiers =
                    initIdentifiers s.state.groups s.state.groupTypes s.state.identifierTypes (Type.TType TType.Group) Nothing a.uuid
                        |> Dict.union (Identifier.fromUuid a.what a.uuid s.state.identifiers)
                , warning = ""
                , step = Step.Step StepType
                , steps = [ Step.Step StepType, Step.Step StepIdentifiers ]
                }
            )
        |> Maybe.withDefault
            { route = f.route
            , flatselect = Nothing
            , uuid = newUuid
            , seed = newSeed
            , identifiers = initIdentifiers s.state.groups s.state.groupTypes s.state.identifierTypes (Type.TType TType.Group) Nothing newUuid
            , warning = ""
            , step = Step.Step StepType
            , steps = [ Step.Step StepType, Step.Step StepIdentifiers ]
            }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputType mat ->
            ( { model
                | flatselect = mat
                , identifiers = initIdentifiers s.state.groups s.state.groupTypes s.state.identifierTypes (Type.TType TType.Group) mat model.uuid
              }
            , Effect.none
            )

        InputIdentifier i ->
            ( { model | identifiers = Dict.insert (Identifier.compare i) i model.identifiers }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))

        Added ->
            case validate model of
                Ok a ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatchMany s
                            (Message.DefinedGroup a
                                :: List.map Message.IdentifierAdded (Dict.values model.identifiers)
                            )
                        , redirectParent s.navkey model.route |> Effect.fromCmd
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
            Ok ()

        Step StepIdentifiers ->
            Ok ()


validate : Model -> Result String Group
validate m =
    case m.flatselect of
        Just at ->
            -- TODO check that TType thing is useful
            Ok <| Group (Type.TType TType.Group) m.uuid at.uuid Empty Dict.empty Dict.empty

        Nothing ->
            Err "You must select a Group Type"


buttonValidate : Model -> Result String field -> Element Msg
buttonValidate m result =
    -- TODO try to suppress using at View.Step.nextMsg
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
                    let
                        allTwithIdentifiers =
                            tWithIdentifiers s.state.identifiers s.state.groups

                        allHwithIdentifiers =
                            hWithIdentifiers s.state.identifiers s.state.groupTypes
                    in
                    column [ alignTop, spacing 10, width <| minimum 200 fill ]
                        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
                            [ h2 "Type"
                            , model.flatselect
                                |> Maybe.map (withIdentifiers s.state.identifiers)
                                |> Maybe.map (hViewHalfCard (InputType Nothing) s.state.groups allHwithIdentifiers s.state.configs)
                                |> Maybe.withDefault (el [ padding 5, Font.color color.text.disabled ] (text "Empty"))
                            ]
                        , h2 "Choose the type of the new Group:"
                        , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ]
                            (allHwithIdentifiers
                                |> Dict.values
                                |> List.map (hClickableCard InputType s.state.groups allHwithIdentifiers s.state.configs)
                                |> withDefaultContent (p "(There are no Group Types yet)")
                            )
                        ]

                Step.Step StepIdentifiers ->
                    let
                        scope =
                            model.flatselect |> Maybe.map (\h -> HasUserType (Type.TType TType.Group) h.uuid) |> Maybe.withDefault (HasType (Type.TType TType.Group))
                    in
                    inputIdentifiers { onEnter = Step.nextMsg model Button Step.NextPage Added, onInput = InputIdentifier } model scope
    in
    floatingContainer s
        "Adding a Group"
        (List.map (Element.map Button) (buttons model (checkStep model))
            ++ [ buttonValidate model (checkStep model) ]
        )
        [ step
        ]
