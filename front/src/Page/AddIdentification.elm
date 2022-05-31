module Page.AddIdentification exposing (..)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import REA.Agent as A exposing (Agent)
import REA.AgentType as AT exposing (AgentType)
import REA.Commitment as CM exposing (Commitment)
import REA.CommitmentType as CMT exposing (CommitmentType)
import REA.Contract as CN exposing (Contract)
import REA.ContractType as CNT exposing (ContractType)
import REA.Entity as EN exposing (toPluralString, toString)
import REA.EntityType as ENT exposing (EntityType, EntityTypes(..))
import REA.EventType as ET exposing (EventType)
import REA.Group as G exposing (Group, compare)
import REA.Ident exposing (Fragment(..), Identification, allFragments, fragmentToDesc, fragmentToName, fragmentToString, setName)
import REA.Process as P exposing (Process)
import REA.ProcessCommitments as PC exposing (ProcessCommitments)
import REA.ProcessEvents as PE exposing (ProcessEvents)
import REA.ProcessType as PT exposing (ProcessType)
import REA.ProcessTypeCommitmentType as PTCT exposing (ProcessTypeCommitmentType)
import REA.ProcessTypeEventType as PTET exposing (ProcessTypeEventType)
import REA.Resource as R exposing (Resource)
import REA.ResourceType as RT exposing (ResourceType)
import Result exposing (andThen)
import Route exposing (Route)
import Shared
import Spa.Page
import State
import Style exposing (..)
import View exposing (..)
import View.Navbar as Navbar
import View.Radio as Radio
import View.Step as Step exposing (isFirst, nextOrValidate, nextStep, previousStep)


type
    Msg
    -- TODO replace with Input Identifier
    = InputName String
    | InputEntity EN.Entity
    | InputEntityTypes (DictSet String String)
    | InputUnique Bool
    | InputMandatory Bool
    | InputFormat (List Fragment)
    | Warning String
    | PreviousPage
    | NextPage
    | Cancel
    | Added


type alias Flags =
    { route : Route }


type alias Model =
    { route : Route
    , name : String
    , entity : Maybe EN.Entity
    , unique : Bool
    , mandatory : Bool
    , fragments : List Fragment
    , applyTo : DictSet String String
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepName
    | StepEntity
    | StepOptions
    | StepFormat
    | StepEntityTypes


validate : Model -> Result String Identification
validate m =
    Result.map2
        -- avoid map6 which doesn't exist:
        (\entity name -> { entity = entity, name = name, fragments = m.fragments, applyTo = ENT.fromSet entity m.applyTo, unique = m.unique, mandatory = m.mandatory })
        (checkNothing m.entity "You must select an entity")
        (checkEmptyString m.name "The name is Empty")


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
    -- TODO give the entity to create through the flags? /add/identification?step=2
    case route of
        Route.AddIdentification ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , name = ""
      , entity = Nothing
      , applyTo = Set.empty identity
      , unique = False
      , mandatory = False
      , fragments = []
      , warning = ""
      , steps = [ Step.Step StepName, Step.Step StepEntity, Step.Step StepEntityTypes, Step.Step StepOptions, Step.Step StepFormat ]
      , step = Step.Step StepName
      }
    , closeMenu s
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputName x ->
            ( { model | name = x }, Effect.none )

        InputEntity x ->
            ( { model
                | entity = Just x
                , applyTo = Set.empty identity
              }
            , Effect.none
            )

        InputEntityTypes ts ->
            ( { model | applyTo = ts }, Effect.none )

        InputUnique x ->
            ( { model | unique = x }, Effect.none )

        InputMandatory x ->
            ( { model | mandatory = x }, Effect.none )

        InputFormat x ->
            ( { model | fragments = x }, Effect.none )

        Warning err ->
            ( { model | warning = err }, Effect.none )

        Added ->
            case validate model of
                Ok i ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatch s <| Event.IdentificationAdded i
                        , redirect s Route.Identifications
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        PreviousPage ->
            case previousStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, redirect s Route.Identifications )

        NextPage ->
            case nextStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, redirect s Route.Identifications )

        Cancel ->
            ( model, redirect s Route.Identifications )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding an Identification"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


buttonNext : Model -> Element Msg
buttonNext model =
    case model.step of
        Step.Step StepEntity ->
            nextOrValidate model NextPage Added (checkNothing model.entity "Please select an Entity")

        Step.Step StepOptions ->
            nextOrValidate model NextPage Added (Ok model.name)

        Step.Step StepFormat ->
            nextOrValidate model NextPage Added (checkEmptyList model.fragments "The format is still empty")

        Step.Step StepName ->
            nextOrValidate model NextPage Added (checkEmptyString model.name "Please choose a name")

        Step.Step StepEntityTypes ->
            nextOrValidate model NextPage Added (Ok model.applyTo)


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
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
                Step.Step StepEntity ->
                    row [ alignTop, width <| minimum 200 fill, Font.size size.text.h3 ]
                        [ Radio.view
                            { title = "Apply to Which Entity?"
                            , options = EN.all |> List.map (\e -> ( e, toString e ))
                            , selected = model.entity
                            , msg =
                                \e -> InputEntity e
                            }
                        ]

                Step.Step StepEntityTypes ->
                    inputEntityTypes s model

                Step.Step StepOptions ->
                    column [ alignTop, width <| minimum 200 fill, spacing 10 ]
                        [ h3 "Options:"
                        , row [ Font.size size.text.main ]
                            [ Input.checkbox
                                []
                                { onChange = InputUnique
                                , icon = Input.defaultCheckbox
                                , checked = model.unique
                                , label = Input.labelRight [] <| text "Each value is unique"
                                }
                            ]
                        , row [ Font.size size.text.main ]
                            [ Input.checkbox
                                []
                                { onChange = InputMandatory
                                , icon = Input.defaultCheckbox
                                , checked = model.mandatory
                                , label = Input.labelRight [] <| text "This identification is mandatory"
                                }
                            ]
                        ]

                Step.Step StepFormat ->
                    inputTags model

                Step.Step StepName ->
                    el [ alignTop ] <|
                        Input.text
                            [ width <| minimum 200 fill
                            , Input.focusedOnLoad
                            , Step.onEnter NextPage Added Warning model (checkEmptyString model.name "Please enter a name")
                            ]
                            { onChange = InputName
                            , text = model.name
                            , placeholder =
                                Just <| Input.placeholder [] <| text "Name"
                            , label = Input.labelAbove [ Font.size size.text.h3, paddingXY 0 10 ] <| text "Give a name to this new identification"
                            }
    in
    cardContent s
        "Adding an identification"
        buttons
        [ step
        ]


viewItem : Model -> String -> Element Msg
viewItem model name =
    row [ Background.color color.item.background ]
        [ el [ paddingXY 10 2 ] (text <| name)
        , button.primary (InputEntityTypes <| Set.remove name model.applyTo) "×"
        ]


inputEntityTypes : Shared.Model -> Model -> Element Msg
inputEntityTypes s model =
    let
        iName =
            model.name

        eType =
            Maybe.withDefault "entity" <| Maybe.map toString model.entity
    in
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (el [ paddingXY 10 0, Font.size size.text.h2 ] <| text "Apply to: ")
                :: List.append
                    (if List.isEmpty model.fragments then
                        [ el [ padding 10, Font.color color.text.disabled ] (text <| "All " ++ eType ++ " types") ]

                     else
                        []
                    )
                    (List.map (\t -> viewItem model t) <| Set.toList model.applyTo)
        , h2 <| "Select the " ++ eType ++ " types that should have a \"" ++ iName ++ "\" identifier"
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
            (State.allNames s.state model.entity
                |> List.map
                    (\et ->
                        column
                            [ Background.color color.item.background
                            , mouseOver itemHoverstyle
                            , pointer
                            , onClick <| InputEntityTypes <| Set.insert et model.applyTo
                            ]
                            [ row [ alignLeft ]
                                [ button.primary (InputEntityTypes <| Set.insert et model.applyTo) "+"
                                , el [ paddingXY 10 0 ] (text et)
                                ]
                            ]
                    )
            )
        ]


inputTags : Model -> Element Msg
inputTags model =
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (el [ paddingXY 10 0 ] <| h2 "Format: ")
                :: List.append
                    (if List.isEmpty model.fragments then
                        [ el [ padding 10, Font.color color.text.disabled ] (text "Empty") ]

                     else
                        []
                    )
                    (model.fragments
                        |> List.indexedMap
                            (\i fragment ->
                                row [ Background.color color.item.background ]
                                    [ el [ paddingXY 10 2 ] (text <| fragmentToString fragment)
                                    , fragmentToName fragment
                                        |> Maybe.map
                                            (\name ->
                                                Input.text [ width (px 75) ]
                                                    { onChange =
                                                        \v ->
                                                            InputFormat
                                                                (model.fragments
                                                                    |> List.indexedMap
                                                                        (\index f ->
                                                                            if index == i then
                                                                                setName v f

                                                                            else
                                                                                f
                                                                        )
                                                                )
                                                    , text = name
                                                    , placeholder =
                                                        Just <| Input.placeholder [] <| text <| fragmentToString fragment
                                                    , label = Input.labelHidden <| name
                                                    }
                                            )
                                        |> Maybe.withDefault none
                                    , button.primary
                                        (InputFormat
                                            (model.fragments
                                                |> List.indexedMap Tuple.pair
                                                |> List.filter (\( j, _ ) -> j /= i)
                                                |> List.map Tuple.second
                                            )
                                        )
                                        "×"
                                    ]
                            )
                    )
        , h2 "Click on the items below to construct the format of your identifier"
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ] <|
            List.map
                (\f ->
                    column
                        [ Background.color color.item.background
                        , mouseOver itemHoverstyle
                        , width (px 250)
                        , onClick (InputFormat <| model.fragments ++ [ f ])
                        , pointer
                        , padding 10
                        , spacing 10
                        , height (px 150)
                        ]
                        [ el [] (text <| fragmentToString f)
                        , paragraph [ Font.size size.text.main ] [ text <| fragmentToDesc model.entity f ]
                        ]
                )
                allFragments
        ]
