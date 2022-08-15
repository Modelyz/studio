module ResourceType.AddPage exposing (..)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable exposing (Groupable)
import Group.Input exposing (inputGroups)
import Hierarchy.Hierarchic as Hierarchic exposing (Hierarchic)
import Hierarchy.Type as HType
import Hierarchy.View exposing (toDesc)
import Ident.Identifiable exposing (hWithIdentifiers)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType exposing (initIdentifiers)
import Ident.Input exposing (inputIdentifiers)
import Item.Item as Item exposing (Item)
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended as Random exposing (Seed, initialSeed)
import ResourceType.ResourceType as ResourceType exposing (ResourceType)
import Route exposing (Route, redirectParent)
import Shared
import Spa.Page
import State exposing (State)
import Type exposing (Type)
import View exposing (..)
import View.Smallcard exposing (hClickableCard, hViewHalfCard, hViewSmallCard)
import View.Step as Step exposing (Step(..), buttons, isLast)
import View.Style exposing (..)


type alias Flags =
    { route : Route }


type alias Model =
    { route : Route
    , uuid : Uuid
    , seed : Seed
    , flatselect : Maybe ResourceType
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
    = InputType (Maybe ResourceType)
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
        Route.ResourceTypeAdd ->
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
      , identifiers = initIdentifiers s.state.resources s.state.resourceTypes s.state.identifierTypes (Type.HType HType.ResourceType) Nothing newUuid
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
        InputType mrt ->
            ( { model
                | flatselect = mrt
                , identifiers = initIdentifiers s.state.resources s.state.resourceTypes s.state.identifierTypes (Type.HType HType.ResourceType) mrt model.uuid
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
                Ok rt ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatchMany s
                            (Message.AddedResourceType rt
                                :: List.map Message.IdentifierAdded (Dict.values model.identifiers)
                                ++ List.map (\g -> Message.Grouped (Groupable.AT rt) g) (Dict.values model.groups)
                            )
                        , redirectParent s.navkey model.route |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding an Resource Type"
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


validate : Model -> Result String ResourceType
validate m =
    Ok <| ResourceType (Type.HType HType.ResourceType) m.uuid (Maybe.map .uuid m.flatselect) Dict.empty


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
                    let
                        allHwithIdentifiers =
                            hWithIdentifiers s.state.identifiers s.state.resourceTypes
                    in
                    column [ alignTop, spacing 10, width <| minimum 200 fill ]
                        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
                            [ h2 "Type"
                            , Maybe.map (hViewHalfCard (InputType Nothing) s.state.resources allHwithIdentifiers s.state.configs) model.flatselect
                                |> Maybe.withDefault (el [ padding 5, Font.color color.text.disabled ] (text "Empty"))
                            ]
                        , h2 "Optional parent type for the new Resource Type (it can be hierarchical)"
                        , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ]
                            (allHwithIdentifiers
                                |> Dict.values
                                |> List.map (hClickableCard InputType s.state.resources allHwithIdentifiers s.state.configs)
                                |> withDefaultContent (p "(There are no Resource Types yet)")
                            )
                        ]

                Step.Step StepGroups ->
                    inputGroups { onInput = InputGroups } s model

                Step.Step StepIdentifiers ->
                    inputIdentifiers { onEnter = Added, onInput = InputIdentifier } model
    in
    floatingContainer s
        "Adding an ResourceType"
        (List.map (Element.map Button) (buttons model (checkStep model))
            ++ [ buttonValidate model (checkStep model) ]
        )
        [ step
        ]
