module Value.AddPage exposing (Flags, Model, Msg(..), Step(..), Subpage, match, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Expression as Expression exposing (BOperator, Expression(..), UOperator)
import Html.Attributes as Attr
import Message
import Route exposing (Route, redirect)
import Scope.Scope as Scope exposing (Scope(..))
import Scope.View exposing (selectScope)
import Shared
import Spa.Page
import Util exposing (checkEmptyString, checkListOne)
import Value.DeepLink as DeepLink exposing (DeepLink)
import Value.DeepLink.Select
import Value.Observable as Obs exposing (Observable(..))
import Value.Rational as R
import Value.Select
import Value.ValueSelection exposing (ValueSelection(..))
import Value.ValueType exposing (ValueType)
import View exposing (..)
import View.Step as Step exposing (Msg(..), Step(..), buttons)
import View.Style exposing (..)


type Msg
    = InputName String
    | InputScope Scope
    | InputMandatory Bool
    | AddExpression Expression
    | InputExpression ( Int, List Int ) Expression
    | UnaryOperator UOperator
    | BinaryOperator BOperator
    | RemoveExpression Int
    | Undo
    | SubMsg Value.Select.Msg
    | SubMsg2 Value.DeepLink.Select.Msg
    | Open Subpage Int (List Int)
    | Warning String
    | Button Step.Msg


type alias Flags =
    { route : Route
    , vtid : String
    }


type alias Model =
    { route : Route
    , name : String
    , mandatory : Bool
    , stack : List Expression
    , scope : Scope
    , subpage : Maybe Subpage
    , submodel : Value.Select.Model
    , submodel2 : Value.DeepLink.Select.Model
    , warning : String
    , steps : List (Step.Step Step)
    , step : Step.Step Step
    , old : Maybe ValueType
    }


type Step
    = StepName
    | StepScope
    | StepOptions
    | StepExpression


type Subpage
    = ValueSelector (ValueSelection -> Msg)
    | DeeplinkSelector (DeepLink -> Msg)


validate : Model -> Result String ValueType
validate m =
    Result.map4 ValueType
        (checkEmptyString m.name "The name is Empty")
        (checkListOne m.stack "Your expression stack must have a single element")
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
            checkListOne model.stack "The stack must contain exactly one expression"
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
            , mandatory = False
            , stack = []
            , scope = Scope.empty
            , subpage = Nothing
            , submodel = Value.Select.init s
            , submodel2 = Value.DeepLink.Select.init s Scope.empty
            , warning = ""
            , steps = [ Step.Step StepName, Step.Step StepScope, Step.Step StepOptions, Step.Step StepExpression ]
            , step = Step.Step StepName
            , old = Nothing
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
                    , stack = [ vt.expr ]
                    , scope = vt.scope
                    , old = Just vt
                }
            )
        |> Maybe.withDefault adding
        |> Effect.with (closeMenu f s.menu)


applyU : UOperator -> List Expression -> List Expression
applyU o stack =
    Maybe.map2 (\h t -> Unary o h :: t) (List.head stack) (List.tail stack) |> Maybe.withDefault []


applyB : BOperator -> List Expression -> List Expression
applyB o stack =
    Maybe.map3 (\f s t -> Binary o f s :: t) (List.head stack) ((List.tail >> Maybe.andThen List.head) stack) ((List.tail >> Maybe.andThen List.tail) stack) |> Maybe.withDefault []


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputName x ->
            ( { model | name = x }, Effect.none )

        InputScope scope ->
            ( { model
                | scope = scope
              }
            , Effect.none
            )

        InputMandatory x ->
            ( { model | mandatory = x }, Effect.none )

        AddExpression expr ->
            ( { model | stack = expr :: model.stack }, Effect.none )

        InputExpression ( stackNum, targetPath ) subExpr ->
            ( { model
                | stack =
                    List.indexedMap
                        (\i e ->
                            if stackNum == i then
                                -- update the expression with the subexpr at given path
                                Expression.updateExpr targetPath [] subExpr e

                            else
                                e
                        )
                        model.stack
              }
            , Effect.none
            )

        UnaryOperator o ->
            ( { model | stack = applyU o model.stack }, Effect.none )

        BinaryOperator o ->
            ( { model | stack = applyB o model.stack }, Effect.none )

        RemoveExpression stackNum ->
            ( { model
                | stack =
                    model.stack
                        |> (List.indexedMap Tuple.pair
                                >> List.filter (Tuple.first >> (/=) stackNum)
                                >> List.map Tuple.second
                           )
              }
            , Effect.none
            )

        Undo ->
            ( { model | stack = undo model.stack }, Effect.none )

        Warning err ->
            ( { model | warning = err }, Effect.none )

        Open (ValueSelector onSelect) _ _ ->
            ( { model
                | subpage = Just (ValueSelector onSelect)
              }
            , Effect.none
            )

        Open (DeeplinkSelector onSelect) _ _ ->
            ( { model
                | subpage = Just (DeeplinkSelector onSelect)
                , submodel2 = Value.DeepLink.Select.init s model.scope
              }
            , Effect.none
            )

        SubMsg sub ->
            case model.subpage of
                Nothing ->
                    ( model, Effect.none )

                Just (ValueSelector _) ->
                    let
                        ( newsubmodel, subcmd ) =
                            Value.Select.update s sub model.submodel
                    in
                    case sub of
                        Value.Select.Cancel ->
                            ( { model | subpage = Nothing }, Effect.fromCmd (Cmd.map SubMsg subcmd) )

                        Value.Select.Selected vs ->
                            -- TODO try to merge with InputExpression
                            case vs of
                                SelectedValue _ _ _ ->
                                    -- TODO we don't use the selected value?
                                    ( { model
                                        | submodel = newsubmodel
                                        , subpage = Nothing
                                        , stack =
                                            List.indexedMap
                                                (\i e ->
                                                    if model.submodel.stackNum == i then
                                                        -- update the expression with the subexpr at given path
                                                        Expression.updateExpr model.submodel.targetPath [] (Leaf <| ObsValue vs) e

                                                    else
                                                        e
                                                )
                                                model.stack
                                      }
                                    , Effect.fromCmd (Cmd.map SubMsg subcmd)
                                    )

                                _ ->
                                    ( model, Effect.none )

                        _ ->
                            ( { model | submodel = newsubmodel }, Effect.fromCmd (Cmd.map SubMsg subcmd) )

                _ ->
                    ( model, Effect.none )

        SubMsg2 sub ->
            case model.subpage of
                Nothing ->
                    ( model, Effect.none )

                Just (DeeplinkSelector _) ->
                    let
                        ( newsubmodel, subcmd ) =
                            Value.DeepLink.Select.update s sub model.submodel2
                    in
                    case sub of
                        Value.DeepLink.Select.Cancel ->
                            ( { model | subpage = Nothing }, Effect.fromCmd (Cmd.map SubMsg2 subcmd) )

                        Value.DeepLink.Select.Choose dl ->
                            -- TODO try to merge with InputExpression
                            ( { model
                                | submodel2 = newsubmodel
                                , subpage = Nothing
                                , stack =
                                    List.indexedMap
                                        (\i e ->
                                            if model.submodel.stackNum == i then
                                                -- update the expression with the subexpr at given path
                                                Expression.updateExpr model.submodel.targetPath [] (Leaf <| ObsLink dl) e

                                            else
                                                e
                                        )
                                        model.stack
                              }
                            , Effect.fromCmd (Cmd.map SubMsg2 subcmd)
                            )

                        _ ->
                            ( { model | submodel2 = newsubmodel }, Effect.fromCmd (Cmd.map SubMsg2 subcmd) )

                _ ->
                    ( model, Effect.none )

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
                    expressionEditor s model

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
        (viewSubpage s model)


viewSubpage : Shared.Model -> Model -> Maybe (Element Msg)
viewSubpage s model =
    Maybe.map
        (\subpage ->
            case subpage of
                ValueSelector _ ->
                    Element.map SubMsg <| Value.Select.view s model.submodel

                DeeplinkSelector _ ->
                    Element.map SubMsg2 <| Value.DeepLink.Select.view s model.submodel2
        )
        model.subpage


buttonUndo : Model -> Element Msg
buttonUndo model =
    if List.length model.stack == 0 then
        button.disabled "" "Undo"

    else
        button.special Undo "Undo"


undo : List Expression -> List Expression
undo stack =
    (List.head stack
        |> Maybe.map
            (\expr ->
                case expr of
                    Leaf _ ->
                        []

                    Unary _ e ->
                        [ e ]

                    Binary _ e1 e2 ->
                        [ e1, e2 ]
            )
        |> Maybe.withDefault []
    )
        ++ (List.tail stack |> Maybe.withDefault [])


expressionEditor : Shared.Model -> Model -> Element Msg
expressionEditor s model =
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ -- display buttons
          wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (buttonUndo model
                :: List.map buttonObservable Obs.allObs
                ++ List.map (buttonUnaryOperator (List.head model.stack)) Expression.allUnary
                ++ List.map (buttonBinaryOperator (Maybe.map2 Tuple.pair (List.head model.stack) ((List.tail >> Maybe.andThen List.head) model.stack))) Expression.allBinary
            )
        , -- display the stack
          column [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            List.indexedMap (displayLine s model) model.stack
        ]


displayLine : Shared.Model -> Model -> Int -> Expression -> Element Msg
displayLine s model stackNum expr =
    row []
        [ row [ height fill, width fill, alignTop, paddingEach { zero | right = 5 } ]
            [ el [ alignLeft ] (button.primary (RemoveExpression stackNum) "×")
            ]
        , editExpression s model stackNum ( [], expr )
        ]


editExpression : Shared.Model -> Model -> Int -> ( List Int, Expression ) -> Element Msg
editExpression s model stackNum ( currentPath, expr ) =
    -- used to modify the expression and input default values
    case expr of
        Leaf obs ->
            editObservable s model ( stackNum, currentPath ) obs

        Unary o e ->
            row [] [ text (Expression.uToShortString o), editExpression s model stackNum ( 1 :: currentPath, e ) ]

        Binary o e1 e2 ->
            row [] [ text "( ", editExpression s model stackNum ( 2 :: currentPath, e1 ), text <| Expression.bToShortString o, editExpression s model stackNum ( 3 :: currentPath, e2 ), text " )" ]


editObservable : Shared.Model -> Model -> ( Int, List Int ) -> Observable -> Element Msg
editObservable s model ( stackNum, exprPath ) obs =
    case obs of
        ObsNumber n ->
            row [ Background.color color.item.background ]
                [ row [ Font.size size.text.small ]
                    [ Input.text [ width (px 70) ]
                        { onChange =
                            \x ->
                                InputExpression ( stackNum, exprPath ) (Leaf <| ObsNumber { n | name = x })
                        , text = n.name
                        , placeholder =
                            Just <| Input.placeholder [] <| text "Name"
                        , label = Input.labelHidden <| "Name"
                        }
                    ]
                , row [ Font.size size.text.small ]
                    [ Input.text [ width (px 70) ]
                        { onChange =
                            \x ->
                                InputExpression ( stackNum, exprPath ) (Leaf <| ObsNumber { n | input = x, val = Result.map Tuple.first <| R.fromString x })
                        , text = n.input
                        , placeholder =
                            Just <| Input.placeholder [] <| text "Default value"
                        , label = Input.labelHidden <| "Default value"
                        }
                    ]
                ]

        ObsValue vs ->
            let
                onSelect : ValueSelection -> Msg
                onSelect =
                    InputExpression ( stackNum, exprPath ) << Leaf << ObsValue
            in
            case vs of
                UndefinedValue ->
                    row [ Background.color color.item.background, Font.size size.text.small, height fill ]
                        [ button.primary (Open (ValueSelector onSelect) stackNum exprPath) "Choose value..."
                        ]

                SelectedValue _ _ name ->
                    row [ Background.color color.item.background, Font.size size.text.small, height fill ]
                        [ button.primary (Open (ValueSelector onSelect) stackNum exprPath) name
                        ]

        ObsLink deeplink ->
            let
                onSelect : DeepLink -> Msg
                onSelect =
                    InputExpression ( stackNum, exprPath ) << Leaf << ObsLink
            in
            case deeplink of
                DeepLink.Null ->
                    row [ Background.color color.item.background, Font.size size.text.small, height fill ]
                        [ button.primary (Open (DeeplinkSelector onSelect) stackNum exprPath) "Choose link..."
                        ]

                DeepLink.EndPoint scope name ->
                    row [ Background.color color.item.background, Font.size size.text.small, height fill, htmlAttribute <| Attr.title <| Scope.toString scope ]
                        [ button.primary (Open (DeeplinkSelector onSelect) stackNum exprPath) name
                        ]

                DeepLink.Link _ _ ->
                    row [ Background.color color.item.background, Font.size size.text.small, height fill ]
                        [ button.primary (Open (DeeplinkSelector onSelect) stackNum exprPath) (DeepLink.toDisplay deeplink)
                        ]


buttonObservable : Observable -> Element Msg
buttonObservable obs =
    case obs of
        ObsNumber n ->
            button.primary (AddExpression <| Leaf (ObsNumber n)) (Obs.toString obs)

        ObsValue v ->
            button.primary (AddExpression <| Leaf (ObsValue v)) (Obs.toString obs)

        ObsLink l ->
            button.primary (AddExpression <| Leaf (ObsLink l)) (Obs.toString obs)


buttonUnaryOperator : Maybe Expression -> UOperator -> Element Msg
buttonUnaryOperator me o =
    Maybe.map (\_ -> button.primary (UnaryOperator o) (Expression.uToString o)) me
        |> Maybe.withDefault (button.disabled "Add one expression in the stack to use this button" (Expression.uToString o))


buttonBinaryOperator : Maybe ( Expression, Expression ) -> BOperator -> Element Msg
buttonBinaryOperator mt o =
    Maybe.map (\_ -> button.primary (BinaryOperator o) (Expression.bToString o)) mt
        |> Maybe.withDefault (button.disabled "Add two expressions in the stack to use this button" (Expression.bToString o))
