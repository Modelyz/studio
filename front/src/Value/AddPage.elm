module Value.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect)
import Scope.Scope exposing (Scope(..))
import Scope.View exposing (selectScope)
import Shared
import Spa.Page
import Task
import Type exposing (Type)
import Value.Rational as R exposing (Rational(..))
import Value.Select
import Value.Value exposing (..)
import Value.ValueType as VT exposing (ValueType)
import View exposing (..)
import View.Smallcard exposing (viewSmallCard)
import View.Step as Step exposing (Msg(..), Step(..), buttons, isLast)
import View.Style exposing (..)


type Msg submsg
    = InputName String
    | InputScope Scope
    | InputMandatory Bool
    | AddExpression Expression
    | InputExpression ( Int, List Int ) Expression
    | UnaryOperator UOperator
    | BinaryOperator BOperator
    | RemoveExpression Int
    | Undo
    | SubMsg submsg
    | Open (Popup submsg) Int (List Int)
    | ClosePopup
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
    , subpage : Maybe (Popup Value.Select.Msg)
    , submodel : Value.Select.Model
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


type Popup submsg
    = PopupSelectValue (ValueSelection -> Msg submsg)


validate : Model -> Result String ValueType
validate m =
    Result.map4 ValueType
        (checkEmptyString m.name "The name is Empty")
        (checkListOne m.stack "Your expression stack must have a single element")
        (if m.scope == Empty then
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
            if model.scope == Empty then
                Err "You must choose a scope"

            else
                Ok ()


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View (Msg Value.Select.Msg)) Model (Msg Value.Select.Msg)
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


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg (Msg submsg) )
init s f =
    let
        adding =
            { route = f.route
            , name = ""
            , mandatory = False
            , stack = []
            , scope = Empty
            , subpage = Nothing
            , submodel = Value.Select.init s
            , warning = ""
            , steps = [ Step.Step StepName, Step.Step StepScope, Step.Step StepOptions, Step.Step StepExpression ]
            , step = Step.Step StepName
            , old = Nothing
            }
    in
    s.state.valueTypes
        |> Dict.filter (\k v -> k == f.vtid)
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


update : Shared.Model -> Msg Value.Select.Msg -> Model -> ( Model, Effect Shared.Msg (Msg Value.Select.Msg) )
update s msg model =
    case msg of
        InputName x ->
            ( { model | name = x }, Effect.none )

        InputScope scope ->
            ( { model | scope = scope }, Effect.none )

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
                                updateExpr targetPath [] subExpr e

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

        Open subpage stackNum targetPath ->
            -- TODO try to pass the submodel along with the subpage (modify the Subpage Type to take the submodel)
            let
                submodel =
                    model.submodel
            in
            ( { model | subpage = Just subpage, submodel = { submodel | stackNum = stackNum, targetPath = targetPath } }, Effect.none )

        ClosePopup ->
            ( { model | subpage = Nothing }, Effect.none )

        SubMsg sub ->
            let
                ( submodel, subcmd ) =
                    Value.Select.update s sub model.submodel
            in
            case sub of
                Value.Select.Cancel ->
                    ( { model | subpage = Nothing }, Effect.fromCmd (Cmd.map SubMsg subcmd) )

                Value.Select.Selected vs ->
                    -- TODO try to merge with InputExpression
                    case model.subpage of
                        Just (PopupSelectValue onSelect) ->
                            case vs of
                                SelectedValue _ _ _ ->
                                    -- TODO we don't use the selected value?
                                    ( { model
                                        | submodel = submodel
                                        , subpage = Nothing
                                        , stack =
                                            List.indexedMap
                                                (\i e ->
                                                    if model.submodel.stackNum == i then
                                                        -- update the expression with the subexpr at given path
                                                        updateExpr model.submodel.targetPath [] (Leaf <| ObsValue vs) e

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
                            ( { model | submodel = submodel, subpage = Nothing }, Effect.fromCmd (Cmd.map SubMsg subcmd) )

                _ ->
                    ( { model | submodel = submodel }, Effect.fromCmd (Cmd.map SubMsg subcmd) )

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


view : Shared.Model -> Model -> View (Msg Value.Select.Msg)
view s model =
    { title = "Adding a ValueType"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element (Msg Value.Select.Msg)
viewContent model s =
    let
        step =
            case model.step of
                Step.Step StepScope ->
                    selectScope s InputScope model.scope

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
        (viewPopup s model)


viewPopup : Shared.Model -> Model -> Maybe (Element (Msg Value.Select.Msg))
viewPopup s model =
    Maybe.map
        (\subpage ->
            case subpage of
                PopupSelectValue onSelect ->
                    Element.map SubMsg <| Value.Select.view s model.submodel
        )
        model.subpage


buttonUndo : Model -> Element (Msg submsg)
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
                    Leaf obs ->
                        []

                    Unary o e ->
                        [ e ]

                    Binary o e1 e2 ->
                        [ e1, e2 ]
            )
        |> Maybe.withDefault []
    )
        ++ (List.tail stack |> Maybe.withDefault [])


expressionEditor : Shared.Model -> Model -> Element (Msg submsg)
expressionEditor s model =
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ -- display buttons
          wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (buttonUndo model
                :: List.map buttonObservable allObs
                ++ List.map (buttonUnaryOperator (List.head model.stack)) allUnary
                ++ List.map (buttonBinaryOperator (Maybe.map2 Tuple.pair (List.head model.stack) ((List.tail >> Maybe.andThen List.head) model.stack))) allBinary
            )
        , -- display the stack
          column [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            List.indexedMap (displayLine s model) model.stack
        ]


displayLine : Shared.Model -> Model -> Int -> Expression -> Element (Msg submsg)
displayLine s model stackNum expr =
    row []
        [ row [ height fill, width fill, alignTop, paddingEach { zero | right = 5 } ]
            [ el [ alignLeft ] (button.primary (RemoveExpression stackNum) "×")
            ]
        , editExpression s model stackNum ( [], expr )
        ]


editExpression : Shared.Model -> Model -> Int -> ( List Int, Expression ) -> Element (Msg submsg)
editExpression s model stackNum ( currentPath, expr ) =
    -- used to modify the expression and input default values
    case expr of
        Leaf obs ->
            editObservable s model ( stackNum, currentPath ) obs

        Unary o e ->
            row [] [ text (uToShortString o), editExpression s model stackNum ( 1 :: currentPath, e ) ]

        Binary o e1 e2 ->
            row [] [ text "( ", editExpression s model stackNum ( 2 :: currentPath, e1 ), text <| bToShortString o, editExpression s model stackNum ( 3 :: currentPath, e2 ), text " )" ]


editObservable : Shared.Model -> Model -> ( Int, List Int ) -> Observable -> Element (Msg submsg)
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

        ObsValue ov ->
            let
                onSelect : ValueSelection -> Msg submsg
                onSelect =
                    InputExpression ( stackNum, exprPath ) << Leaf << ObsValue
            in
            case ov of
                UndefinedValue ->
                    row [ Background.color color.item.background, Font.size size.text.small, height fill ]
                        [ button.primary (Open (PopupSelectValue onSelect) stackNum exprPath) "Choose value..."
                        ]

                SelectedValue _ _ name ->
                    row [ Background.color color.item.background, Font.size size.text.small, height fill ]
                        [ button.primary (Open (PopupSelectValue onSelect) stackNum exprPath) name
                        ]


buttonObservable : Observable -> Element (Msg submsg)
buttonObservable obs =
    case obs of
        ObsNumber n ->
            button.primary (AddExpression <| Leaf (ObsNumber n)) (toString obs)

        ObsValue v ->
            button.primary (AddExpression <| Leaf (ObsValue v)) (toString obs)


buttonUnaryOperator : Maybe Expression -> UOperator -> Element (Msg submsg)
buttonUnaryOperator me o =
    Maybe.map (\e -> button.primary (UnaryOperator o) (uToString o)) me
        |> Maybe.withDefault (button.disabled "Add one expression in the stack to use this button" (uToString o))


buttonBinaryOperator : Maybe ( Expression, Expression ) -> BOperator -> Element (Msg submsg)
buttonBinaryOperator mt o =
    Maybe.map (\t -> button.primary (BinaryOperator o) (bToString o)) mt
        |> Maybe.withDefault (button.disabled "Add two expressions in the stack to use this button" (bToString o))
