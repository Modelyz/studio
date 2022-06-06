module View.AddEntityType exposing (..)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Event
import REA.EntityType as ENT exposing (EntityType(..), onlyType)
import REA.Type exposing (Type)
import Route exposing (Route)
import Shared
import Style exposing (..)
import View exposing (..)
import View.FlatSelect exposing (flatselect)
import View.Step as Step exposing (isFirst, nextOrValidate, nextStep, previousStep)


type Msg
    = InputName String
    | InputType (Maybe EntityType)
    | InputRestrict EntityType
    | InputUnrestrict EntityType
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
    | StepProcesses


type alias Model =
    { route : Route
    , name : String
    , flatselect : Maybe EntityType
    , processTypes : DictSet String EntityType
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type alias Config =
    { filter : DictSet String EntityType -> DictSet String EntityType
    , typeExplain : String
    , nameExplain : String
    , pageTitle : String
    , processRestriction : String
    , typeConstructor : Type -> EntityType
    }


view : Config -> Shared.Model -> Model -> View Msg
view c s model =
    { title = c.pageTitle
    , attributes = []
    , element = viewContent c model
    , route = model.route
    }


validate : Config -> Model -> Result String EntityType
validate c m =
    Result.map3
        (\n p pts -> c.typeConstructor { name = n, parent = p |> Maybe.map ENT.toName })
        (checkEmptyString m.name "The name is Empty")
        (Ok m.flatselect)
        (Ok m.processTypes)


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , flatselect = Nothing
      , name = ""
      , processTypes = Set.empty ENT.compare
      , warning = ""
      , steps = [ Step.Step StepName, Step.Step StepType, Step.Step StepProcesses ]
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

        InputRestrict pt ->
            ( { model | processTypes = Set.insert pt model.processTypes }, Effect.none )

        InputUnrestrict pt ->
            ( { model | processTypes = Set.remove pt model.processTypes }, Effect.none )

        Warning err ->
            ( { model | warning = err }, Effect.none )

        Added ->
            case validate c model of
                Ok t ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatchMany s <| Event.TypeAdded t :: List.map (\pt -> Event.Restricted { what = t, scope = pt }) (Set.toList model.processTypes)
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

        Step.Step StepProcesses ->
            nextOrValidate model NextPage Added (Ok model.processTypes)


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
                        { all = Set.toList <| c.filter <| s.state.entityTypes
                        , toString = ENT.toName
                        , toDesc = ENT.toParent
                        , onInput = InputType
                        , label = "Type: "
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

                Step.Step StepProcesses ->
                    column
                        [ spacing 10 ]
                        [ p c.processRestriction
                        , column [ spacing 10 ]
                            (s.state.entityTypes
                                |> Set.filter (\et -> ENT.toString et == "ProcessType")
                                |> Set.toList
                                |> List.sortBy ENT.compare
                                |> List.map
                                    (\pt ->
                                        row []
                                            [ row []
                                                [ Input.checkbox
                                                    []
                                                    { onChange =
                                                        \b ->
                                                            if b then
                                                                InputRestrict pt

                                                            else
                                                                InputUnrestrict pt
                                                    , icon = Input.defaultCheckbox
                                                    , checked = Set.member pt model.processTypes
                                                    , label = Input.labelRight [] <| text (ENT.toName pt)
                                                    }
                                                ]
                                            ]
                                    )
                            )
                        ]
    in
    cardContent s
        c.pageTitle
        buttons
        [ step
        ]
