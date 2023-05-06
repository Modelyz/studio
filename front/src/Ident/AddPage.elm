module Ident.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import Ident.Fragment as Fragment exposing (Fragment(..))
import Ident.IdentifierType exposing (IdentifierType)
import Message
import Route exposing (Route, redirect)
import Scope exposing (Scope)
import Scope.View exposing (selectScope)
import Shared
import Spa.Page
import Util exposing (checkEmptyList, checkEmptyString)
import View exposing (..)
import View.MultiSelect exposing (multiSelect)
import View.Step as Step exposing (Msg(..), Step(..), buttons)
import View.Style exposing (..)


type Msg
    = InputName String
    | InputScope Scope
    | InputUnique Bool
    | InputMandatory Bool
    | InputFragments (List Fragment)
    | Warning String
    | Button Step.Msg


type alias Flags =
    { route : Route, itid : String }


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
    , old : Maybe IdentifierType
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
        (if m.scope == Scope.empty then
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
            if model.scope == Scope.empty then
                Err "You must choose a scope"

            else
                Ok ()


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init s
        , update = update s
        , view = view
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    -- TODO give the entity to create through the flags? /add/identifierType?step=2
    case route of
        Route.Entity Route.IdentifierType (Route.Add _ _) ->
            Just { route = route, itid = "" }

        Route.Entity Route.IdentifierType (Route.Edit itid _) ->
            Just { route = route, itid = itid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        adding =
            { route = f.route
            , name = ""
            , scope = Scope.anything
            , unique = False
            , mandatory = False
            , fragments = []
            , warning = ""
            , steps = [ Step.Step StepScope, Step.Step StepName, Step.Step StepOptions, Step.Step StepFormat ]
            , step = Step.Step StepScope
            , old = Nothing
            }
    in
    s.state.identifierTypes
        |> Dict.filter (\k _ -> k == f.itid)
        |> Dict.values
        |> List.head
        |> Maybe.map
            (\it ->
                { adding
                    | name = it.name
                    , unique = it.unique
                    , mandatory = it.mandatory
                    , fragments = it.fragments
                    , scope = it.scope
                    , old = Just it
                }
            )
        |> Maybe.withDefault adding
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

        Button Step.Added ->
            case validate model of
                Ok i ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatch s <|
                            Maybe.withDefault (Message.AddedIdentifierType i) <|
                                Maybe.map (Message.ChangedIdentifierType i) model.old
                        , Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.IdentifierType (Route.List Nothing)
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))


view : Model -> View Msg
view model =
    { title = "Adding an Identifier Type"
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
                    selectScope s.state InputScope model.scope Scope.anything "Add an identifier to:"

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
                    multiSelect
                        model
                        { inputMsg = InputFragments
                        , selection = .fragments
                        , title = "Format: "
                        , description = "Click on the items below to construct the format of the identifier"
                        , toString = Fragment.toString
                        , toDesc = Fragment.toDesc
                        , empty = "No identifier available"
                        , height = 100
                        , input = inputFragment
                        }
                        Fragment.all

                Step.Step StepName ->
                    el [ alignTop ] <|
                        Input.text
                            [ width <| minimum 200 fill
                            , Input.focusedOnLoad
                            , Step.onEnter (Button NextPage) (Button Added) Warning model (checkEmptyString model.name "Please enter a name" |> Result.map (\_ -> ()))
                            ]
                            { onChange = InputName
                            , text = model.name
                            , placeholder =
                                Just <| Input.placeholder [] <| text "Name"
                            , label = Input.labelAbove [ Font.size size.text.h3, paddingXY 0 10 ] <| text "Give a name to this new Identifier Type"
                            }
    in
    floatingContainer s
        (Just <| Button Step.Cancel)
        "Adding an Identifier Type"
        (List.map (Element.map Button) (buttons model (checkStep model)))
        [ step
        ]


inputFragment : List Fragment -> Int -> Fragment -> Element Msg
inputFragment fragments index fragment =
    let
        attrId =
            htmlAttribute <| Attr.id ("segment" ++ "/" ++ String.fromInt index)
    in
    -- TODO refactor to
    case fragment of
        Fixed value ->
            Input.text
                [ width (px 75)
                , attrId
                ]
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

        Existing e ->
            Input.text
                [ width (px 75)
                , attrId
                ]
                { onChange =
                    \n ->
                        InputFragments
                            (fragments
                                |> List.indexedMap
                                    (\i f ->
                                        if i == index then
                                            Existing { e | value = n }

                                        else
                                            f
                                    )
                            )
                , text = e.value
                , placeholder =
                    Just <| Input.placeholder [] <| text <| Fragment.toString fragment
                , label = Input.labelHidden <| "Existing"
                }

        DateFrom d ->
            Input.text [ width (px 75), attrId ]
                { onChange =
                    \n ->
                        InputFragments
                            (fragments
                                |> List.indexedMap
                                    (\i f ->
                                        if i == index then
                                            DateFrom { d | field = n }

                                        else
                                            f
                                    )
                            )
                , text = d.field
                , placeholder =
                    Just <| Input.placeholder [] <| text <| Fragment.toString fragment
                , label = Input.labelHidden <| "Existing"
                }

        Sequence seq ->
            row []
                [ Input.text [ width (px 50), htmlAttribute <| Attr.title "name", attrId ]
                    { onChange =
                        \name ->
                            InputFragments
                                (fragments
                                    |> List.indexedMap
                                        (\i f ->
                                            if i == index then
                                                Sequence { seq | name = name }

                                            else
                                                f
                                        )
                                )
                    , text = seq.name
                    , placeholder =
                        Just <| Input.placeholder [] <| text "Name"
                    , label = Input.labelHidden <| "Name"
                    }
                , Input.text [ width (px 50), htmlAttribute <| Attr.title "padding", attrId ]
                    { onChange =
                        \x ->
                            InputFragments
                                (fragments
                                    |> List.indexedMap
                                        (\i f ->
                                            if i == index then
                                                String.toInt x
                                                    |> Maybe.map (\padding -> Sequence { seq | padding = padding })
                                                    |> Maybe.withDefault (Sequence seq)

                                            else
                                                f
                                        )
                                )
                    , text = String.fromInt seq.padding
                    , placeholder =
                        Just <| Input.placeholder [] <| text "Padding"
                    , label = Input.labelHidden <| "Padding"
                    }
                , Input.text [ width (px 50), htmlAttribute <| Attr.title "step", attrId ]
                    { onChange =
                        \x ->
                            InputFragments
                                (fragments
                                    |> List.indexedMap
                                        (\i f ->
                                            if i == index then
                                                String.toInt x
                                                    |> Maybe.map (\step -> Sequence { seq | step = step })
                                                    |> Maybe.withDefault (Sequence seq)

                                            else
                                                f
                                        )
                                )
                    , text = String.fromInt seq.step
                    , placeholder =
                        Just <| Input.placeholder [] <| text "Step"
                    , label = Input.labelHidden <| "Step"
                    }
                , Input.text [ width (px 50), htmlAttribute <| Attr.title "start", attrId ]
                    { onChange =
                        \x ->
                            InputFragments
                                (fragments
                                    |> List.indexedMap
                                        (\i f ->
                                            if i == index then
                                                String.toInt x
                                                    |> Maybe.map (\start -> Sequence { seq | start = start })
                                                    |> Maybe.withDefault (Sequence seq)

                                            else
                                                f
                                        )
                                )
                    , text = String.fromInt seq.start
                    , placeholder =
                        Just <| Input.placeholder [] <| text "First value"
                    , label = Input.labelHidden <| "First value"
                    }
                ]

        _ ->
            none
