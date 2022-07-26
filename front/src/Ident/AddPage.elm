module Ident.AddPage exposing (..)

import Configuration exposing (Configuration(..))
import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Entity.Entity as Entity
import Entity.Type as Type exposing (Type(..))
import Ident.Fragment as Fragment exposing (Fragment(..))
import Ident.IdentifierType exposing (IdentifierType)
import Ident.Scope as Scope exposing (Scope(..))
import Ident.View
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import View exposing (..)
import View.Lang exposing (Lang(..))
import View.Step as Step exposing (isFirst, nextOrValidate, nextStep, previousStep)
import View.Style exposing (..)
import View.Type exposing (Type(..))
import Zone.Zone as Zone exposing (Zone(..))


type
    Msg
    -- TODO replace with Input Identifier
    = InputName String
    | InputScope (Maybe Scope)
    | InputUnique Bool
    | InputMandatory Bool
    | InputFragments (List Fragment)
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
    , applyTo : Maybe Scope
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = StepName
    | StepScope
    | StepOptions
    | StepFormat


validate : Model -> Result String IdentifierType
validate m =
    Result.map5 IdentifierType
        (checkEmptyString m.name "The name is Empty")
        (checkEmptyList m.fragments "Your identifier format is empty")
        (checkNothing m.applyTo "You must select an entity type")
        (Ok m.unique)
        (Ok m.mandatory)


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
        Route.IdentifierTypeAdd ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    { route = f.route
    , name = ""
    , applyTo = Nothing
    , unique = False
    , mandatory = False
    , fragments = []
    , warning = ""
    , steps = [ Step.Step StepName, Step.Step StepScope, Step.Step StepOptions, Step.Step StepFormat ]
    , step = Step.Step StepName
    }
        |> Effect.with (closeMenu f s.menu)


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputName x ->
            ( { model | name = x }, Effect.none )

        InputScope scope ->
            ( { model | applyTo = scope }, Effect.none )

        InputUnique x ->
            ( { model | unique = x }, Effect.none )

        InputMandatory x ->
            ( { model | mandatory = x }, Effect.none )

        InputFragments x ->
            ( { model | fragments = x }, Effect.none )

        Warning err ->
            ( { model | warning = err }, Effect.none )

        Added ->
            case validate model of
                Ok i ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatch s <| Message.IdentifierTypeAdded i
                        , redirect s.navkey Route.IdentifierTypeList |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        PreviousPage ->
            case previousStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, redirect s.navkey Route.IdentifierTypeList |> Effect.fromCmd )

        NextPage ->
            case nextStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, redirect s.navkey Route.IdentifierTypeList |> Effect.fromCmd )

        Cancel ->
            ( model, redirect s.navkey Route.IdentifierTypeList |> Effect.fromCmd )


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

        Step.Step StepScope ->
            nextOrValidate model NextPage Added (checkNothing model.applyTo "Please choose an option")


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
                Step.Step StepScope ->
                    inputScopes s model

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
                    inputFragments model

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
    floatingContainer s
        "Adding an identifierType"
        buttons
        [ step
        ]


viewItem : Shared.Model -> Model -> Scope -> Element Msg
viewItem s model scope =
    row [ Background.color color.item.background ]
        [ el [ paddingXY 10 2 ] (Ident.View.displayScope s scope)
        , button.primary (InputScope Nothing) "×"
        ]


inputScopes : Shared.Model -> Model -> Element Msg
inputScopes s model =
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (el [ paddingXY 10 0, Font.size size.text.h2 ] <| text "Apply to: ")
                :: [ Maybe.map (viewItem s model) model.applyTo |> Maybe.withDefault none ]
        , h2 <| "Select the types of the entities that should have a \"" ++ model.name ++ "\" identifier"
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
            (Type.all
                |> List.map
                    (\t ->
                        clickableCard (InputScope <| Just <| AllEntities t) (text <| "All " ++ Type.toPluralString t) none
                    )
            )
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
            (s.state.entities
                |> Entity.onlyTypes
                |> Set.toList
                |> List.map
                    (\e ->
                        [ (Type.fromType <| Entity.toType e)
                            |> Maybe.map
                                (\t ->
                                    [ clickableCard
                                        (InputScope <| Just <| AllEntitiesOfType t (Entity.toUuid e))
                                        (Ident.View.displayScope s (AllEntitiesOfType t (Entity.toUuid e)))
                                        (Entity.toTypeUuid e
                                            |> Maybe.andThen (Entity.fromUuid s.state.entities)
                                            |> Maybe.map (\p -> row [ Font.size size.text.small ] [ text "Type: ", Ident.View.display s SmallcardItemTitle FR_fr p ])
                                            |> Maybe.withDefault none
                                        )
                                    ]
                                )
                            |> Maybe.withDefault []
                        , (Type.toType <| Entity.toType e)
                            |> Maybe.map
                                (\t ->
                                    [ clickableCard
                                        (InputScope <| Just <| AllEntitiesOfType (Entity.toType e) (Entity.toUuid e))
                                        (Ident.View.displayScope s (AllEntitiesOfType t (Entity.toUuid e)))
                                        (Entity.toTypeUuid e
                                            |> Maybe.andThen (Entity.fromUuid s.state.entities)
                                            |> Maybe.map (\p -> row [ Font.size size.text.small ] [ text "Type: ", Ident.View.display s SmallcardItemTitle FR_fr p ])
                                            |> Maybe.withDefault none
                                        )
                                    ]
                                )
                            |> Maybe.withDefault []
                        ]
                    )
                |> List.concat
                |> List.concat
            )
        ]


inputFragments : Model -> Element Msg
inputFragments model =
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
                                    [ button.primary
                                        (InputFragments
                                            (model.fragments
                                                |> List.indexedMap Tuple.pair
                                                |> List.filter (\( j, _ ) -> j /= i)
                                                |> List.map Tuple.second
                                            )
                                        )
                                        "×"
                                    , el [ paddingXY 10 2 ] (text <| Fragment.toString fragment)
                                    , inputFragment model.fragments i fragment
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
                        , onClick (InputFragments <| model.fragments ++ [ f ])
                        , pointer
                        , padding 10
                        , spacing 10
                        , height (px 150)
                        ]
                        [ el [] (text <| Fragment.toString f)
                        , paragraph [ Font.size size.text.main ] [ text <| Fragment.toDesc f ]
                        ]
                )
                Fragment.all
        ]


inputFragment : List Fragment -> Int -> Fragment -> Element Msg
inputFragment fragments index fragment =
    case fragment of
        Fixed value ->
            Input.text [ width (px 75) ]
                { onChange =
                    \v ->
                        InputFragments
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
                    Just <| Input.placeholder [] <| text <| Fragment.toString fragment
                , label = Input.labelHidden <| "Fixed"
                }

        Existing name value ->
            Input.text [ width (px 75) ]
                { onChange =
                    \n ->
                        InputFragments
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
                    Just <| Input.placeholder [] <| text <| Fragment.toString fragment
                , label = Input.labelHidden <| "Existing"
                }

        DateFrom name posix ->
            Input.text [ width (px 75) ]
                { onChange =
                    \n ->
                        InputFragments
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
                    Just <| Input.placeholder [] <| text <| Fragment.toString fragment
                , label = Input.labelHidden <| "Existing"
                }

        Sequence padding step start value ->
            row []
                [ Input.text [ width (px 50) ]
                    { onChange =
                        \x ->
                            InputFragments
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
                            InputFragments
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
                            InputFragments
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
