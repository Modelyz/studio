module Value.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Expression as Expression exposing (BOperator, Expression(..), UOperator)
import Expression.DeepLink as DeepLink exposing (DeepLink)
import Expression.DeepLink.Select
import Expression.Editor exposing (view)
import Expression.Observable as Obs exposing (Observable(..))
import Html.Attributes as Attr
import Message
import Route exposing (Route, redirect)
import Scope.Scope as Scope exposing (Scope(..))
import Scope.View exposing (selectScope)
import Shared
import Spa.Page
import Util exposing (checkEmptyString, checkListOne)
import Value.Select
import Value.ValueSelection exposing (ValueSelection(..))
import Value.ValueType exposing (ValueType)
import View exposing (..)
import View.Step as Step exposing (Msg(..), Step(..), buttons)
import View.Style exposing (..)



{- Page used to define a ValueType -}


type Msg
    = InputName String
    | InputScope Scope
    | InputMandatory Bool
    | EditorMsg Expression.Editor.Msg
    | Warning String
    | Button Step.Msg


type alias Flags =
    { route : Route
    , vtid : String
    }


type alias Model =
    { route : Route
    , name : String
    , scope : Scope
    , mandatory : Bool
    , warning : String
    , steps : List (Step.Step Step)
    , step : Step.Step Step
    , old : Maybe ValueType
    , editor : Expression.Editor.Model
    }


type Step
    = StepName
    | StepScope
    | StepOptions
    | StepExpression


validate : Model -> Result String ValueType
validate m =
    Result.map4 ValueType
        (checkEmptyString m.name "The name is Empty")
        (Expression.Editor.checkExpression m.editor)
        (if m.scope == Scope.empty then
            Err "You must choose a scope"

         else
            Ok m.scope
        )
        (Ok m.mandatory)


checkStep : Model -> Result String ()
checkStep model =
    case model.step of
        Step.Step StepOptions ->
            Ok ()

        Step.Step StepExpression ->
            Expression.Editor.checkExpression model.editor
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
        , view = view s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    -- TODO give the entity to create through the flags? /add/valueType?step=2
    case route of
        Route.Entity Route.ValueType Route.Add ->
            Just { route = route, vtid = "" }

        Route.Entity Route.ValueType (Route.Edit vtid) ->
            Just { route = route, vtid = vtid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        adding =
            { route = f.route
            , name = ""
            , scope = Scope.empty
            , mandatory = False
            , warning = ""
            , steps = [ Step.Step StepName, Step.Step StepScope, Step.Step StepOptions, Step.Step StepExpression ]
            , step = Step.Step StepName
            , old = Nothing
            , editor = Expression.Editor.init s Scope.empty []
            }
    in
    s.state.valueTypes
        |> Dict.filter (\k _ -> k == f.vtid)
        |> Dict.values
        |> List.head
        |> Maybe.map
            (\vt ->
                { adding
                    | name = vt.name
                    , mandatory = vt.mandatory
                    , old = Just vt
                    , scope = vt.scope
                    , editor = Expression.Editor.init s vt.scope [ vt.expr ]
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
            ( { model
                | scope = scope
                , editor = Expression.Editor.init s scope []
              }
            , Effect.none
            )

        InputMandatory x ->
            ( { model | mandatory = x }, Effect.none )

        Warning err ->
            ( { model | warning = err }, Effect.none )

        Button Step.Added ->
            case validate model of
                Ok i ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatch s <|
                            Maybe.withDefault (Message.AddedValueType i) <|
                                Maybe.map (Message.ChangedValueType i) model.old
                        , Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.ValueType (Route.List Nothing)
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))

        EditorMsg editormsg ->
            Expression.Editor.update s editormsg model.editor
                |> (\( x, y ) -> ( { model | editor = x }, Effect.map EditorMsg <| Effect.fromCmd y ))


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding a ValueType"
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
                    selectScope s InputScope model.scope Scope.empty "What should it apply to?"

                Step.Step StepOptions ->
                    column [ alignTop, width <| minimum 200 fill, spacing 10 ]
                        [ h3 "Options:"
                        , row [ Font.size size.text.main ]
                            [ Input.checkbox
                                []
                                { onChange = InputMandatory
                                , icon = Input.defaultCheckbox
                                , checked = model.mandatory
                                , label = Input.labelRight [] <| text "This valueType is mandatory"
                                }
                            ]
                        ]

                Step.Step StepExpression ->
                    Element.map EditorMsg <| Expression.Editor.view s model.editor

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
                            , label = Input.labelAbove [ Font.size size.text.h3, paddingXY 0 10 ] <| text "Give a name to this new valueType"
                            }
    in
    floatingContainer2 s
        (Just <| Button Step.Cancel)
        "Adding an valueType"
        (List.map (Element.map Button) (buttons model (checkStep model)))
        [ step
        ]
        (Maybe.map (Element.map EditorMsg) <| Expression.Editor.viewSubpage s model.editor)
