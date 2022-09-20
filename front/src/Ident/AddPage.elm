module Ident.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Ident.Fragment as Fragment exposing (Fragment(..))
import Ident.IdentifierType exposing (IdentifierType)
import Message
import Route exposing (Route, redirect)
import Scope.Scope exposing (Scope(..))
import Scope.View exposing (inputScope)
import Shared
import Spa.Page
import View exposing (..)
import View.Step as Step exposing (Msg(..), Step(..), buttons, isLast)
import View.Style exposing (..)


type Msg
    = InputName String
    | InputScope Scope
    | InputUnique Bool
    | InputMandatory Bool
    | InputFragments (List Fragment)
    | Warning String
    | Added
    | Button Step.Msg


type alias Flags =
    { route : Route }


type alias Model =
    { route : Route
    , name : String
    , unique : Bool
    , mandatory : Bool
    , fragments : List Fragment
    , scope : Scope
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
        (if m.scope == Empty then
            Err "You must choose a scope"

         else
            Ok m.scope
        )
        (Ok m.unique)
        (Ok m.mandatory)


checkStep : Model -> Result String ()
checkStep model =
    case model.step of
        Step.Step StepOptions ->
            Ok ()

        Step.Step StepFormat ->
            checkEmptyList model.fragments "The format is still empty"
                |> Result.map (\_ -> ())

        Step.Step StepName ->
            checkEmptyString model.name "Please choose a name"
                |> Result.map (\_ -> ())

        Step.Step StepScope ->
            if model.scope == Empty then
                Err "You must choose a scope"

            else
                Ok ()


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
    , scope = Empty
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
            ( { model | scope = scope }, Effect.none )

        InputUnique x ->
            ( { model | unique = x }, Effect.none )

        InputMandatory x ->
            ( { model | mandatory = x }, Effect.none )

        InputFragments x ->
            ( { model | fragments = x }, Effect.none )

        Warning err ->
            ( { model | warning = err }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))

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


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding an IdentifierType"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepScope ->
                    inputScope s InputScope model

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
                            , Step.onEnter (Button NextPage) Added Warning model (checkEmptyString model.name "Please enter a name" |> Result.map (\_ -> ()))
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
        (List.map (Element.map Button) (buttons model (checkStep model))
            ++ [ buttonValidate model (checkStep model) ]
        )
        [ step
        ]


buttonValidate : Model -> Result String field -> Element Msg
buttonValidate m result =
    case result of
        Ok _ ->
            if isLast m.step m.steps then
                button.primary Added "Validate and finish"

            else
                none

        Err _ ->
            none


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

        Existing _ value ->
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
