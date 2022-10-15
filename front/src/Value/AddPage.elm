module Value.AddPage exposing (Flags, Model, Msg(..), Step(..), match, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import GroupType.ListPage
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect)
import Scope.Scope exposing (Scope(..))
import Scope.View exposing (inputScope)
import Shared
import Spa.Page
import Type exposing (Type)
import Value.Expression as Expression exposing (Expression(..))
import Value.Observable as Observable exposing (Observable(..))
import Value.Rational exposing (Rational(..))
import Value.ValueType exposing (ValueType)
import View exposing (..)
import View.Smallcard exposing (viewSmallCard)
import View.Step as Step exposing (Msg(..), Step(..), buttons, isLast)
import View.Style exposing (..)


type Msg submsg
    = InputName String
    | InputScope Scope
    | InputMandatory Bool
    | AddExpression (Expression Observable)
    | InputExpression ( Int, List Int ) (Expression Observable)
    | UnaryOperator Expression.UOperator
    | BinaryOperator Expression.BOperator
    | RemoveExpression Int
    | Undo
    | Ask SubPage
    | OverMsg submsg
    | Warning String
    | Added
    | Button Step.Msg


type alias Flags =
    { route : Route }


type alias Model =
    { route : Route
    , name : String
    , mandatory : Bool
    , stack : List (Expression Observable)
    , scope : Scope
    , subpage : SubPage
    , submodel : GroupType.ListPage.Model
    , warning : String
    , steps : List (Step.Step Step)
    , step : Step.Step Step
    }


type Step
    = StepName
    | StepScope
    | StepOptions
    | StepExpression


type SubPage
    = SubPageClosed
    | SubPageType GroupType.ListPage.Model
    | SubPageUuid GroupType.ListPage.Model
    | SubPageName GroupType.ListPage.Model


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


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View (Msg GroupType.ListPage.Msg)) Model (Msg GroupType.ListPage.Msg)
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
        Route.ValueTypeAdd ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg (Msg GroupType.ListPage.Msg) )
init s f =
    { route = f.route
    , name = ""
    , mandatory = False
    , stack = []
    , scope = Empty
    , subpage = SubPageClosed
    , submodel = Tuple.first <| GroupType.ListPage.init s { route = f.route }
    , warning = ""
    , steps = [ Step.Step StepName, Step.Step StepScope, Step.Step StepOptions, Step.Step StepExpression ]
    , step = Step.Step StepName
    }
        |> Effect.with (closeMenu f s.menu)


updateExpr : List Int -> List Int -> Expression Observable -> Expression Observable -> Expression Observable
updateExpr targetPath currentPath subExpr expr =
    -- we replace the expr at the given path
    case expr of
        Leaf obs ->
            if currentPath == targetPath then
                subExpr

            else
                expr

        Unary o e ->
            Unary o (updateExpr targetPath (1 :: currentPath) subExpr e)

        Binary o e1 e2 ->
            Binary o (updateExpr targetPath (2 :: currentPath) subExpr e1) (updateExpr targetPath (3 :: currentPath) subExpr e2)


applyU : Expression.UOperator -> List (Expression Observable) -> List (Expression Observable)
applyU o stack =
    Maybe.map2 (\h t -> Unary o h :: t) (List.head stack) (List.tail stack) |> Maybe.withDefault []


applyB : Expression.BOperator -> List (Expression Observable) -> List (Expression Observable)
applyB o stack =
    Maybe.map3 (\f s t -> Binary o f s :: t) (List.head stack) ((List.tail >> Maybe.andThen List.head) stack) ((List.tail >> Maybe.andThen List.tail) stack) |> Maybe.withDefault []


update : Shared.Model -> Msg GroupType.ListPage.Msg -> Model -> ( Model, Effect Shared.Msg (Msg GroupType.ListPage.Msg) )
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

        InputExpression ( stacknum, targetPath ) subExpr ->
            ( { model
                | stack =
                    List.indexedMap
                        (\i e ->
                            if stacknum == i then
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

        RemoveExpression stacknum ->
            ( { model
                | stack =
                    model.stack
                        |> (List.indexedMap Tuple.pair
                                >> List.filter (Tuple.first >> (/=) stacknum)
                                >> List.map Tuple.second
                           )
              }
            , Effect.none
            )

        Undo ->
            ( { model | stack = undo model.stack }, Effect.none )

        Warning err ->
            ( { model | warning = err }, Effect.none )

        Button stepmsg ->
            Step.update s stepmsg model
                |> (\( x, y ) -> ( x, Effect.map Button y ))

        Ask subpage ->
            ( { model | subpage = subpage }, Effect.none )

        Added ->
            case validate model of
                Ok i ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatch s <| Message.ValueTypeAdded i
                        , redirect s.navkey Route.ValueTypeList |> Effect.fromCmd
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        OverMsg submsg ->
            ( { model | submodel = Tuple.first <| GroupType.ListPage.update s submsg model.submodel }, Effect.none )


view : Shared.Model -> Model -> View (Msg GroupType.ListPage.Msg)
view s model =
    { title = "Adding a ValueType"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element (Msg GroupType.ListPage.Msg)
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
                            , Step.onEnter (Button NextPage) Added Warning model (checkEmptyString model.name "Please enter a name" |> Result.map (\_ -> ()))
                            ]
                            { onChange = InputName
                            , text = model.name
                            , placeholder =
                                Just <| Input.placeholder [] <| text "Name"
                            , label = Input.labelAbove [ Font.size size.text.h3, paddingXY 0 10 ] <| text "Give a name to this new valueType"
                            }
    in
    floatingContainer s
        "Adding an valueType"
        (List.map (Element.map Button) (buttons model (checkStep model))
            ++ [ buttonValidate model (checkStep model) ]
        )
        [ step
        , Element.map OverMsg <| GroupType.ListPage.viewContent model.submodel s
        ]


buttonValidate : Model -> Result String field -> Element (Msg GroupType.ListPage.Msg)
buttonValidate m result =
    case result of
        Ok _ ->
            if isLast m.step m.steps then
                button.primary Added "Validate and finish"

            else
                none

        Err _ ->
            none


buttonUndo : Model -> Element (Msg GroupType.ListPage.Msg)
buttonUndo model =
    if List.length model.stack == 0 then
        button.disabled "" "Undo"

    else
        button.special Undo "Undo"


undo : List (Expression Observable) -> List (Expression Observable)
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


expressionEditor : Shared.Model -> Model -> Element (Msg GroupType.ListPage.Msg)
expressionEditor s model =
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ -- display buttons
          wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (buttonUndo model
                :: List.map buttonObservable Expression.allObs
                ++ List.map (buttonUnaryOperator (List.head model.stack)) Expression.allUnary
                ++ List.map (buttonBinaryOperator (Maybe.map2 Tuple.pair (List.head model.stack) ((List.tail >> Maybe.andThen List.head) model.stack))) Expression.allBinary
            )
        , -- display the stack
          column [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            List.indexedMap (displayLine s model) model.stack
        ]


displayLine : Shared.Model -> Model -> Int -> Expression Observable -> Element (Msg GroupType.ListPage.Msg)
displayLine s model stacknum expr =
    row []
        [ row [ height fill, width fill, alignTop, paddingEach { edges | right = 5 } ]
            [ el [ alignLeft ] (button.primary (RemoveExpression stacknum) "×")
            ]
        , displayExpression s model stacknum ( [], expr )
        ]


displayExpression : Shared.Model -> Model -> Int -> ( List Int, Expression Observable ) -> Element (Msg GroupType.ListPage.Msg)
displayExpression s model stacknum ( currentPath, expr ) =
    case expr of
        Leaf obs ->
            displayObservable s model ( stacknum, currentPath ) obs

        Unary o e ->
            row [] [ text (Expression.uToShortString o), displayExpression s model stacknum ( 1 :: currentPath, e ) ]

        Binary o e1 e2 ->
            row [] [ text "( ", displayExpression s model stacknum ( 2 :: currentPath, e1 ), text <| Expression.bToShortString o, displayExpression s model stacknum ( 3 :: currentPath, e2 ), text " )" ]


displayObservable : Shared.Model -> Model -> ( Int, List Int ) -> Observable -> Element (Msg GroupType.ListPage.Msg)
displayObservable s model ( stacknum, exprPath ) obs =
    case obs of
        Number n ->
            row []
                [ column [ Background.color color.item.background ]
                    [ row [ Background.color color.item.background ]
                        [ row [ Font.size size.text.small ]
                            [ Input.text [ width (px 70) ]
                                { onChange =
                                    \x ->
                                        InputExpression ( stacknum, exprPath ) (Leaf <| Number { n | val = String.toInt x |> Result.fromMaybe "invalid number" })
                                , text = Result.map String.fromInt n.val |> Result.withDefault ""
                                , placeholder =
                                    Just <| Input.placeholder [] <| text "Default value"
                                , label = Input.labelHidden <| "Default value"
                                }
                            ]
                        , row [ Font.size size.text.small ]
                            [ Input.text [ width (px 70) ]
                                { onChange =
                                    \x ->
                                        InputExpression ( stacknum, exprPath ) (Leaf <| Number { n | name = x })
                                , text = n.name
                                , placeholder =
                                    Just <| Input.placeholder [] <| text "Name"
                                , label = Input.labelHidden <| "Name"
                                }
                            ]
                        ]
                    , row [ Background.color color.item.background ]
                        [ Input.text [ width (px 140) ]
                            { onChange =
                                \x ->
                                    InputExpression ( stacknum, exprPath ) (Leaf <| Number { n | desc = x })
                            , text = n.desc
                            , placeholder =
                                Just <| Input.placeholder [] <| text "Description"
                            , label = Input.labelHidden <| "Description"
                            }
                        ]
                    ]
                ]

        Value mv ->
            row [ Background.color color.item.background, Font.size size.text.small, height fill ]
                [ button.primary (Ask (SubPageName model.submodel)) (mv |> Maybe.map Uuid.toString |> Maybe.withDefault "Choose value...")
                ]


buttonObservable : Observable -> Element (Msg GroupType.ListPage.Msg)
buttonObservable obs =
    case obs of
        Number n ->
            button.primary (AddExpression <| Leaf (Number n)) (Observable.toString obs)

        Value v ->
            button.primary (AddExpression <| Leaf (Value v)) (Observable.toString obs)


buttonUnaryOperator : Maybe (Expression Observable) -> Expression.UOperator -> Element (Msg GroupType.ListPage.Msg)
buttonUnaryOperator me o =
    Maybe.map (\e -> button.primary (UnaryOperator o) (Expression.uToString o)) me
        |> Maybe.withDefault (button.disabled "Add one expression in the stack to use this button" (Expression.uToString o))


buttonBinaryOperator : Maybe ( Expression Observable, Expression Observable ) -> Expression.BOperator -> Element (Msg GroupType.ListPage.Msg)
buttonBinaryOperator mt o =
    Maybe.map (\t -> button.primary (BinaryOperator o) (Expression.bToString o)) mt
        |> Maybe.withDefault (button.disabled "Add two expressions in the stack to use this button" (Expression.bToString o))
