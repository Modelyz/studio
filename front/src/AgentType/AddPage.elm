module AgentType.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import AgentType.AgentType exposing (AgentType)
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
import Type
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
    , flatselect : Maybe AgentType
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
    = InputType (Maybe AgentType)
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
        Route.AgentTypeAdd ->
            Just { route = route, uuid = Nothing }

        Route.AgentTypeEdit uuid ->
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
        |> Maybe.andThen (H.find s.state.agentTypes)
        |> Maybe.map
            (\at ->
                let
                    oldGroups =
                        s.state.grouped
                            |> Dict.filter (\_ v -> at.uuid == Groupable.uuid v.groupable)
                            |> Dict.foldl (\_ v d -> Dict.insert (Group.compare v.group) v.group d) Dict.empty
                in
                { route = f.route
                , flatselect = at.parent |> Maybe.andThen (H.find s.state.agentTypes)
                , uuid = at.uuid
                , seed = newSeed
                , identifiers =
                    initIdentifiers s.state.agents s.state.agentTypes s.state.identifierTypes (Type.HType HType.AgentType) Nothing at.uuid
                        |> Dict.union (Identifier.fromUuid at.uuid s.state.identifiers)
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
            , uuid = newUuid
            , seed = newSeed
            , identifiers = initIdentifiers s.state.agents s.state.agentTypes s.state.identifierTypes (Type.HType HType.AgentType) Nothing newUuid
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
                , identifiers = initIdentifiers s.state.agents s.state.agentTypes s.state.identifierTypes (Type.HType HType.AgentType) mat model.uuid
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
                Ok at ->
                    let
                        addedGroups =
                            Dict.diff model.groups model.oldGroups

                        removedGroups =
                            Dict.diff model.oldGroups model.groups
                    in
                    ( model
                    , Effect.batch
                        [ Shared.dispatchMany s
                            (Message.AddedAgentType at
                                :: List.map Message.IdentifierAdded (Dict.values model.identifiers)
                                ++ List.map (\g -> Message.Grouped (Groupable.AT at) g) (Dict.values addedGroups)
                                ++ List.map (\g -> Message.Ungrouped (Groupable.AT at) g) (Dict.values removedGroups)
                            )
                        , redirectParent s.navkey model.route |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding an Agent Type"
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

        Step StepGroups ->
            Ok ()


validate : Model -> Result String AgentType
validate m =
    Ok <| AgentType (Type.HType HType.AgentType) m.uuid (Maybe.map .uuid m.flatselect) Dict.empty Dict.empty Dict.empty


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
                            s.state.agentTypes |> Dict.map (\_ h -> { h | identifiers = s.state.identifiers |> Dict.filter (\_ id -> h.uuid == id.identifiable) })
                    in
                    column [ alignTop, spacing 10, width <| minimum 200 fill ]
                        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
                            [ h2 "Type"
                            , model.flatselect
                                |> Maybe.map (withIdentifiers s.state.agents s.state.agentTypes s.state.identifierTypes s.state.identifiers)
                                |> Maybe.map (hViewHalfCard (InputType Nothing) s.state.agents allHwithIdentifiers s.state.configs)
                                |> Maybe.withDefault (el [ padding 5, Font.color color.text.disabled ] (text "Empty"))
                            ]
                        , h2 "Optional parent type for the new Agent Type (it can be hierarchical)"
                        , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ]
                            (allHwithIdentifiers
                                |> Dict.values
                                |> List.map (hClickableCard InputType s.state.agents allHwithIdentifiers s.state.configs)
                                |> withDefaultContent (p "(There are no Agent Types yet)")
                            )
                        ]

                Step.Step StepGroups ->
                    inputGroups { onInput = InputGroups } s model

                Step.Step StepIdentifiers ->
                    let
                        scope =
                            model.flatselect |> Maybe.map (\h -> HasUserType (Type.HType HType.AgentType) h.uuid) |> Maybe.withDefault (HasType (Type.HType HType.AgentType))
                    in
                    inputIdentifiers { onEnter = Step.nextMsg model Button Step.NextPage Added, onInput = InputIdentifier } model scope
    in
    floatingContainer s
        "Adding an AgentType"
        (List.map (Element.map Button) (buttons model (checkStep model))
            ++ [ buttonValidate model (checkStep model) ]
        )
        [ step
        ]
