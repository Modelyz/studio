module Page.AddIdentifierType exposing (..)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Event
import Prng.Uuid as Uuid exposing (Uuid)
import REA.Entity as EN exposing (toType, toUuid)
import REA.EntityType as ENT exposing (EntityType(..))
import REA.Ident as Ident exposing (Fragment(..), Identified(..), IdentifierType, allFragments, fragmentToDesc, fragmentToName, fragmentToString)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.Step as Step exposing (isFirst, nextOrValidate, nextStep, previousStep)


type
    Msg
    -- TODO replace with Input Identifier
    = InputName String
    | InputIdentified (DictSet String Identified)
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
    , unique : Bool
    , mandatory : Bool
    , fragments : List Fragment
    , applyTo : DictSet String Identified
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepName
    | StepIdentified
    | StepOptions
    | StepFormat


validate : Model -> Result String IdentifierType
validate m =
    Result.map
        -- avoid map6 which doesn't exist:
        (\name -> { name = name, applyTo = m.applyTo, fragments = m.fragments, unique = m.unique, mandatory = m.mandatory })
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
    -- TODO give the entity to create through the flags? /add/identifierType?step=2
    case route of
        Route.AddIdentifierType ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , name = ""
      , applyTo = Set.empty Ident.compare
      , unique = False
      , mandatory = False
      , fragments = []
      , warning = ""
      , steps = [ Step.Step StepName, Step.Step StepIdentified, Step.Step StepOptions, Step.Step StepFormat ]
      , step = Step.Step StepName
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputName x ->
            ( { model | name = x }, Effect.none )

        InputIdentified i ->
            ( { model | applyTo = i }, Effect.none )

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
                        [ Shared.dispatch s <| Event.IdentifierTypeAdded i
                        , redirect s.navkey Route.IdentifierTypes |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        PreviousPage ->
            case previousStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, redirect s.navkey Route.IdentifierTypes |> Effect.fromCmd )

        NextPage ->
            case nextStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, redirect s.navkey Route.IdentifierTypes |> Effect.fromCmd )

        Cancel ->
            ( model, redirect s.navkey Route.IdentifierTypes |> Effect.fromCmd )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding an IdentifierType"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


buttonNext : Model -> Element Msg
buttonNext model =
    case model.step of
        Step.Step StepOptions ->
            nextOrValidate model NextPage Added (Ok model.name)

        Step.Step StepFormat ->
            nextOrValidate model NextPage Added (checkEmptyList model.fragments "The format is still empty")

        Step.Step StepName ->
            nextOrValidate model NextPage Added (checkEmptyString model.name "Please choose a name")

        Step.Step StepIdentified ->
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
                Step.Step StepIdentified ->
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
                                , label = Input.labelRight [] <| text "This identifierType is mandatory"
                                }
                            ]
                        ]

                Step.Step StepFormat ->
                    inputFragmentConfs model

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
                            , label = Input.labelAbove [ Font.size size.text.h3, paddingXY 0 10 ] <| text "Give a name to this new identifierType"
                            }
    in
    cardContent s
        "Adding an identifierType"
        buttons
        [ step
        ]


viewItem : Model -> Identified -> Element Msg
viewItem model i =
    row [ Background.color color.item.background ]
        [ el [ paddingXY 10 2 ] (text <| Ident.toDesc i)
        , button.primary (InputIdentified <| Set.remove i model.applyTo) "×"
        ]


inputEntityTypes : Shared.Model -> Model -> Element Msg
inputEntityTypes s model =
    let
        iName =
            model.name
    in
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (el [ paddingXY 10 0, Font.size size.text.h2 ] <| text "Apply to: ")
                :: List.append
                    (if Set.isEmpty model.applyTo then
                        [ el [ padding 10, Font.color color.text.disabled ] (text <| "All types") ]

                     else
                        []
                    )
                    (model.applyTo |> Set.toList |> List.map (viewItem model))
        , h2 <| "Select the entities that should have a \"" ++ iName ++ "\" identifier"
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
            (Set.toList s.state.entities
                |> List.map
                    (\e ->
                        clickableCard (InputIdentified <| Set.insert (OneEntity e) model.applyTo) (Uuid.toString <| toUuid e) (Just <| "Type: " ++ toType e)
                    )
            )
        , h2 <| "Select the entity types that should have a \"" ++ iName ++ "\" identifier"
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
            (Set.toList s.state.entityTypes
                |> List.map
                    (\et ->
                        clickableCard (InputIdentified <| Set.insert (OneEntityType et) model.applyTo) (ENT.toName et) (Just <| "Type: " ++ (ENT.toType >> .name) et)
                    )
            )
        , h2 <| "Select the types of the entities that should have a \"" ++ iName ++ "\" identifier"
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
            (Set.toList s.state.entityTypes
                |> List.map
                    (\et ->
                        clickableCard (InputIdentified <| Set.insert (AllEntities et) model.applyTo) (ENT.toName et) (Just <| "Type: " ++ (ENT.toType >> .name) et)
                    )
            )
        , h2 <| "Select the types of the entity types that should have a \"" ++ iName ++ "\" identifier"
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
            (Set.toList s.state.entityTypes
                |> List.map
                    (\et ->
                        clickableCard (InputIdentified <| Set.insert (AllEntityTypes et) model.applyTo) (ENT.toName et) (Just <| "Type: " ++ (ENT.toType >> .name) et)
                    )
            )
        ]


inputFragmentConfs : Model -> Element Msg
inputFragmentConfs model =
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
                                    , inputFragmentConf model.fragments i fragment
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
                        , paragraph [ Font.size size.text.main ] [ text <| fragmentToDesc f ]
                        ]
                )
                allFragments
        ]


inputFragmentConf : List Fragment -> Int -> Fragment -> Element Msg
inputFragmentConf fragments index fragment =
    case fragment of
        Fixed value ->
            Input.text [ width (px 75) ]
                { onChange =
                    \v ->
                        InputFormat
                            (fragments
                                |> List.indexedMap
                                    (\i f ->
                                        if i == index then
                                            Fixed v

                                        else
                                            f
                                    )
                            )
                , text = value
                , placeholder =
                    Just <| Input.placeholder [] <| text <| fragmentToString fragment
                , label = Input.labelHidden <| "Fixed"
                }

        Existing name value ->
            Input.text [ width (px 75) ]
                { onChange =
                    \n ->
                        InputFormat
                            (fragments
                                |> List.indexedMap
                                    (\i f ->
                                        if i == index then
                                            Existing n value

                                        else
                                            f
                                    )
                            )
                , text = value
                , placeholder =
                    Just <| Input.placeholder [] <| text <| fragmentToString fragment
                , label = Input.labelHidden <| "Existing"
                }

        OtherIdentifier name ->
            Input.text [ width (px 75) ]
                { onChange =
                    \n ->
                        InputFormat
                            (fragments
                                |> List.indexedMap
                                    (\i f ->
                                        if i == index then
                                            OtherIdentifier n

                                        else
                                            f
                                    )
                            )
                , text = name
                , placeholder =
                    Just <| Input.placeholder [] <| text <| fragmentToString fragment
                , label = Input.labelHidden <| "Other identifier"
                }

        DateFrom name posix ->
            Input.text [ width (px 75) ]
                { onChange =
                    \n ->
                        InputFormat
                            (fragments
                                |> List.indexedMap
                                    (\i f ->
                                        if i == index then
                                            DateFrom n posix

                                        else
                                            f
                                    )
                            )
                , text = name
                , placeholder =
                    Just <| Input.placeholder [] <| text <| fragmentToString fragment
                , label = Input.labelHidden <| "Existing"
                }

        Sequence padding step start value ->
            row []
                [ Input.text [ width (px 50) ]
                    { onChange =
                        \x ->
                            InputFormat
                                (fragments
                                    |> List.indexedMap
                                        (\i f ->
                                            if i == index then
                                                String.toInt x
                                                    |> Maybe.map (\p -> Sequence p step start value)
                                                    |> Maybe.withDefault (Sequence padding step start value)

                                            else
                                                f
                                        )
                                )
                    , text = String.fromInt padding
                    , placeholder =
                        Just <| Input.placeholder [] <| text "Padding"
                    , label = Input.labelHidden <| "Padding"
                    }
                , Input.text [ width (px 50) ]
                    { onChange =
                        \x ->
                            InputFormat
                                (fragments
                                    |> List.indexedMap
                                        (\i f ->
                                            if i == index then
                                                String.toInt x
                                                    |> Maybe.map (\s -> Sequence padding s start value)
                                                    |> Maybe.withDefault (Sequence padding step start value)

                                            else
                                                f
                                        )
                                )
                    , text = String.fromInt step
                    , placeholder =
                        Just <| Input.placeholder [] <| text "Step"
                    , label = Input.labelHidden <| "Step"
                    }
                , Input.text [ width (px 50) ]
                    { onChange =
                        \x ->
                            InputFormat
                                (fragments
                                    |> List.indexedMap
                                        (\i f ->
                                            if i == index then
                                                String.toInt x
                                                    |> Maybe.map (\s -> Sequence padding step s value)
                                                    |> Maybe.withDefault (Sequence padding step start value)

                                            else
                                                f
                                        )
                                )
                    , text = String.fromInt start
                    , placeholder =
                        Just <| Input.placeholder [] <| text "First value"
                    , label = Input.labelHidden <| "First value"
                    }
                ]

        _ ->
            none
