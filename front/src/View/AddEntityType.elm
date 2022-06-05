module View.AddEntityType exposing (..)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import REA.EntityType as ENT exposing (EntityType(..))
import Result exposing (andThen)
import Route exposing (Route)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.FlatSelect exposing (flatselect)
import View.Navbar as Navbar
import View.Radio as Radio
import View.Step as Step exposing (isFirst, nextOrValidate, nextStep, previousStep)


type Msg
    = InputName String
    | InputType (Maybe EntityType)
    | Warning String
    | PreviousPage
    | NextPage
    | Cancel
    | Added


type alias Flags =
    { route : Route }


type Step
    = StepName
    | StepType


type alias Model =
    { route : Route
    , name : String
    , flatselect : Maybe EntityType
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type alias Config =
    { typeExplain : String
    , nameExplain : String
    , pageTitle : String
    , validate : Model -> Result String EntityType
    }


view : Config -> Shared.Model -> Model -> View Msg
view c s model =
    { title = c.pageTitle
    , attributes = []
    , element = viewContent c model
    , route = model.route
    }


validate : Model -> Result String EntityType
validate m =
    Result.map2
        (\n p -> AgentType { name = n, parent = p |> Maybe.map ENT.toName })
        (checkEmptyString m.name "The name is Empty")
        (Ok m.flatselect)


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , flatselect = Nothing
      , name = ""
      , warning = ""
      , steps = [ Step.Step StepName, Step.Step StepType ]
      , step = Step.Step StepName
      }
    , closeMenu s
    )


update : Config -> Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update c s msg model =
    case msg of
        InputName x ->
            ( { model | name = x }, Effect.none )

        InputType x ->
            ( { model | flatselect = x }, Effect.none )

        Warning err ->
            ( { model | warning = err }, Effect.none )

        Added ->
            case c.validate model of
                Ok t ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatch s <| Event.TypeAdded t
                        , redirectParent s.navkey model.route
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        PreviousPage ->
            case previousStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, redirectParent s.navkey model.route )

        NextPage ->
            case nextStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, redirectParent s.navkey model.route )

        Cancel ->
            ( model, redirectParent s.navkey model.route )


buttonNext : Model -> Element Msg
buttonNext model =
    case model.step of
        Step.Step StepType ->
            nextOrValidate model NextPage Added (Ok model.flatselect)

        Step.Step StepName ->
            nextOrValidate model NextPage Added (checkEmptyString model.name "Please choose a name")


viewContent : Config -> Model -> Shared.Model -> Element Msg
viewContent c model s =
    let
        buttons : List (Element Msg)
        buttons =
            [ wrappedRow [ width fill, spacing 20 ]
                [ (if isFirst model.step model.steps then
                    button.disabled "This is the first page"

                   else
                    button.secondary PreviousPage
                  )
                    "← Previous"
                , button.secondary Cancel "Cancel"
                , buttonNext model
                , if model.warning /= "" then
                    paragraph [ Font.color color.text.warning ] [ text model.warning ]

                  else
                    none
                ]
            ]

        step =
            case model.step of
                Step.Step StepType ->
                    flatselect model
                        { all = Set.toList <| s.state.entityTypes
                        , toString = ENT.toName
                        , toDesc = ENT.toParent
                        , onInput = InputType
                        , label = "Type"
                        , explain = h2 c.typeExplain
                        }

                Step.Step StepName ->
                    el [ alignTop ] <|
                        Input.text
                            [ width <| minimum 200 fill
                            , Input.focusedOnLoad
                            , View.onEnter NextPage
                            ]
                            { onChange = InputName
                            , text = model.name
                            , placeholder =
                                Just <| Input.placeholder [] <| text "Name"
                            , label = Input.labelAbove [ Font.size size.text.h3, paddingXY 0 10 ] <| text c.nameExplain
                            }
    in
    cardContent s
        c.pageTitle
        buttons
        [ step
        ]
