module Expression.Editor exposing (Model, Msg, checkExpression, init, update, view, viewSubpage)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Expression exposing (Expression(..))
import Expression.Binary as B
import Expression.DeepLink as DeepLink
import Expression.DeepLink.Select
import Expression.DeepLink.View
import Expression.Observable as Obs exposing (Observable(..))
import Expression.Rational as Rational
import Expression.RationalInput as RationalInput
import Expression.Unary as U
import Expression.Value.Select
import Expression.ValueSelection exposing (ValueSelection(..))
import Html.Attributes as Attr
import Scope exposing (Scope(..))
import Shared
import Util exposing (checkListOne, otherwise)
import Value.Value as Value exposing (..)
import View exposing (..)
import View.Style exposing (..)


type Msg
    = AddExpression Expression
    | InputExpression ( Int, List Int ) Expression
    | UnaryOperator U.Operator
    | BinaryOperator B.Operator
    | RemoveExpression Int
    | Undo
    | Swap
    | VlMsg Expression.Value.Select.Msg
    | DlMsg Expression.DeepLink.Select.Msg
    | OpenValueSelector Int (List Int)
    | OpenDeepLinkSelector Int (List Int)


type alias Model =
    { scope : Scope
    , stack : List Expression
    , vlselector : Maybe Expression.Value.Select.Model
    , dlselector : Maybe Expression.DeepLink.Select.Model
    }


checkExpression : Model -> Result String Expression
checkExpression model =
    checkListOne model.stack "Your expression stack must have a single element"


init : Scope -> List Expression -> Model
init scope stack =
    { scope = scope
    , stack = stack
    , vlselector = Nothing
    , dlselector = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddExpression expr ->
            ( { model | stack = expr :: model.stack }, Cmd.none )

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
            , Cmd.none
            )

        UnaryOperator o ->
            ( { model | stack = Expression.applyUnary o model.stack }, Cmd.none )

        BinaryOperator o ->
            ( { model | stack = Expression.applyBinary o model.stack }, Cmd.none )

        RemoveExpression stackNum ->
            ( { model
                | stack =
                    model.stack
                        |> (List.indexedMap Tuple.pair
                                >> List.filter (Tuple.first >> (/=) stackNum)
                                >> List.map Tuple.second
                           )
              }
            , Cmd.none
            )

        Undo ->
            ( { model | stack = Expression.undo model.stack }, Cmd.none )

        Swap ->
            ( { model | stack = Expression.swap model.stack }, Cmd.none )

        OpenValueSelector stackNum targetPath ->
            ( { model
                | vlselector = Just <| Expression.Value.Select.init stackNum targetPath
              }
            , Cmd.none
            )

        VlMsg Expression.Value.Select.Cancel ->
            ( { model | vlselector = Nothing }, Cmd.none )

        VlMsg (Expression.Value.Select.Choose vs stackNum targetPath) ->
            case vs of
                SelectedValue _ _ _ ->
                    -- TODO we don't use the selected value?
                    ( { model
                        | vlselector = Nothing
                        , stack =
                            List.indexedMap
                                (\i e ->
                                    if stackNum == i then
                                        -- update the expression with the subexpr at given path
                                        Expression.updateExpr targetPath [] (Leaf <| ObsValue vs) e

                                    else
                                        e
                                )
                                model.stack
                      }
                    , Cmd.none
                    )

                UndefinedValue ->
                    ( model, Cmd.none )

        VlMsg vlmsg ->
            model.vlselector
                |> Maybe.map
                    (\vlselector ->
                        let
                            ( newsubmodel, subcmd ) =
                                Expression.Value.Select.update vlmsg vlselector
                        in
                        ( { model | vlselector = Just newsubmodel }, Cmd.map VlMsg subcmd )
                    )
                |> Maybe.withDefault ( { model | vlselector = Nothing }, Cmd.none )

        OpenDeepLinkSelector stackNum targetPath ->
            ( { model
                | dlselector = Just <| Expression.DeepLink.Select.init model.scope stackNum targetPath
              }
            , Cmd.none
            )

        DlMsg Expression.DeepLink.Select.Cancel ->
            ( { model | dlselector = Nothing }, Cmd.none )

        DlMsg (Expression.DeepLink.Select.Choose dl stackNum targetPath) ->
            ( { model
                | dlselector = Nothing
                , stack =
                    List.indexedMap
                        (\i e ->
                            if stackNum == i then
                                -- update the expression with the subexpr at given path
                                Expression.updateExpr targetPath [] (Leaf <| ObsLink dl) e

                            else
                                e
                        )
                        model.stack
              }
            , Cmd.none
            )

        DlMsg dlmsg ->
            model.dlselector
                |> Maybe.map
                    (\dlselector ->
                        let
                            ( newsubmodel, subcmd ) =
                                Expression.DeepLink.Select.update dlmsg dlselector
                        in
                        ( { model | dlselector = Just newsubmodel }, Cmd.map DlMsg subcmd )
                    )
                |> Maybe.withDefault ( { model | vlselector = Nothing }, Cmd.none )


view : Shared.Model -> Model -> Element Msg
view s model =
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ -- display buttons
          wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            ([ buttonUndo model, buttonSwap model ]
                ++ List.map buttonObservable Obs.allObs
                ++ List.map (buttonUnaryOperator (List.head model.stack)) U.all
                ++ List.map (buttonBinaryOperator (Maybe.map2 Tuple.pair (List.head model.stack) ((List.tail >> Maybe.andThen List.head) model.stack))) B.all
            )
        , -- display the stack
          column [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            List.indexedMap (displayLine s model) model.stack
        ]


displayLine : Shared.Model -> Model -> Int -> Expression -> Element Msg
displayLine s model stackNum expr =
    row []
        [ row [ height fill, width fill, alignTop, paddingEach { zero | right = 5 } ]
            [ el [ alignLeft ] (button.primary (Ok <| RemoveExpression stackNum) "×")
            ]
        , editExpression s model stackNum ( [], expr )
        ]


editExpression : Shared.Model -> Model -> Int -> ( List Int, Expression ) -> Element Msg
editExpression s model stackNum ( currentPath, expr ) =
    -- used to modify the expression and input default values
    case expr of
        Leaf obs ->
            editObservable s ( stackNum, currentPath ) obs

        Unary o e ->
            row [] [ text (U.toShortString o), editExpression s model stackNum ( 1 :: currentPath, e ) ]

        Binary o e1 e2 ->
            row [] [ text "( ", editExpression s model stackNum ( 2 :: currentPath, e1 ), text <| B.toShortString o, editExpression s model stackNum ( 3 :: currentPath, e2 ), text " )" ]


editObservable : Shared.Model -> ( Int, List Int ) -> Observable -> Element Msg
editObservable s ( stackNum, exprPath ) obs =
    case obs of
        ObsNumber n ->
            row [ Background.color color.item.background ]
                [ row [ Font.size size.text.small ]
                    [ Input.text
                        [ width (px 70)
                        , htmlAttribute <| Attr.id ("var" :: (stackNum :: exprPath |> List.map String.fromInt) |> String.join "/")
                        ]
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
                    [ RationalInput.inputText
                        Rational.fromString
                        ("def" :: (stackNum :: exprPath |> List.map String.fromInt) |> String.join "/")
                        (Just "Default value")
                        (\x -> InputExpression ( stackNum, exprPath ) (Leaf <| ObsNumber { n | input = x }))
                        n.input
                    ]
                ]

        ObsValue vs ->
            case vs of
                UndefinedValue ->
                    row [ Background.color color.item.background, Font.size size.text.small, height fill ]
                        [ button.primary (Ok <| OpenValueSelector stackNum exprPath) "Choose value..."
                        ]

                SelectedValue _ _ name ->
                    row [ Background.color color.item.background, Font.size size.text.small, height fill ]
                        [ button.primary (Ok <| OpenValueSelector stackNum exprPath) name
                        ]

        ObsLink deeplink ->
            case deeplink of
                DeepLink.Null ->
                    row [ Background.color color.item.background, Font.size size.text.small, height fill ]
                        [ button.primary (Ok <| OpenDeepLinkSelector stackNum exprPath) "Choose link..."
                        ]

                DeepLink.EndPoint scope name ->
                    row [ Background.color color.item.background, Font.size size.text.small, height fill, htmlAttribute <| Attr.title <| Scope.toString scope ]
                        [ button.primary (Ok <| OpenDeepLinkSelector stackNum exprPath) name
                        ]

                DeepLink.Link _ _ ->
                    row [ Background.color color.item.background, Font.size size.text.small, height fill ]
                        [ button.primary (Ok <| OpenDeepLinkSelector stackNum exprPath) (Expression.DeepLink.View.toDisplay s.state deeplink)
                        ]


buttonObservable : Observable -> Element Msg
buttonObservable obs =
    case obs of
        ObsNumber n ->
            button.primary (Ok <| AddExpression <| Leaf (ObsNumber n)) (Obs.toString obs)

        ObsValue v ->
            button.primary (Ok <| AddExpression <| Leaf (ObsValue v)) (Obs.toString obs)

        ObsLink l ->
            button.primary (Ok <| AddExpression <| Leaf (ObsLink l)) (Obs.toString obs)


buttonUnaryOperator : Maybe Expression -> U.Operator -> Element Msg
buttonUnaryOperator me o =
    Maybe.map (\_ -> button.primary (Ok <| UnaryOperator o) (U.toString o)) me
        |> Maybe.withDefault (button.disabled (Err "Add one expression in the stack to use this button") (U.toString o))


buttonBinaryOperator : Maybe ( Expression, Expression ) -> B.Operator -> Element Msg
buttonBinaryOperator mt o =
    Maybe.map (\_ -> button.primary (Ok <| BinaryOperator o) (B.toString o)) mt
        |> Maybe.withDefault (button.disabled (Err "Add two expressions in the stack to use this button") (B.toString o))


viewSubpage : Shared.Model -> Model -> Maybe (Element Msg)
viewSubpage s model =
    (model.dlselector |> Maybe.map (Element.map DlMsg << Expression.DeepLink.Select.view s))
        |> otherwise (model.vlselector |> Maybe.map (Element.map VlMsg << Expression.Value.Select.view s))


buttonUndo : Model -> Element Msg
buttonUndo model =
    if List.length model.stack == 0 then
        button.disabled (Err "You cannot undo nothing") "Undo"

    else
        button.special (Ok Undo) "Undo"


buttonSwap : Model -> Element Msg
buttonSwap model =
    if List.length model.stack < 2 then
        button.disabled (Err "You must have two elements to swap") "Swap"

    else
        button.special (Ok Swap) "Swap"
