module View.InputIdentifiers exposing (..)

import DictSet as Set exposing (DictSet)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import REA.Ident exposing (Fragment(..), Identifier, Name, fragmentToName, fragmentToString, fragmentToValue, getIdentifier, identifierValue, updateIdentifier)
import Style exposing (size)
import View


type alias Model a =
    { a | identifiers : DictSet String Identifier }


type alias Config msg =
    { onEnter : msg
    , onInput : Identifier -> msg
    }


inputIdentifiers : Config msg -> Model a -> Element msg
inputIdentifiers c model =
    -- display an input field for each relevant identifier
    column [ spacing 10 ]
        (model.identifiers
            |> Set.toList
            |> List.map
                (\identifier ->
                    column []
                        [ el [ paddingXY 0 10 ] <| text identifier.name
                        , row [ spacing 5 ] <|
                            List.indexedMap (\i f -> inputFragment c model i f identifier) identifier.fragments
                        ]
                )
        )


inputFragment : Config msg -> Model a -> Int -> Fragment -> Identifier -> Element msg
inputFragment c model index fragment ident =
    case fragment of
        Free value ->
            Input.text
                [ width <| minimum 200 fill
                , Input.focusedOnLoad
                , View.onEnter c.onEnter
                ]
                { onChange = \v -> c.onInput <| updateIdentifier index (Free v) ident
                , text = value
                , placeholder =
                    Just <| Input.placeholder [] <| text ident.name
                , label = Input.labelHidden ident.name
                }

        Fixed value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text value ]

        OtherIdentifier name ->
            getIdentifier name model.identifiers
                |> Maybe.map (\i -> String.join " " <| List.map fragmentToValue i.fragments)
                |> Maybe.withDefault ("(Error in this identifier: " ++ name ++ " does not exist)")
                |> text

        Sequence padding step start value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text <| Maybe.withDefault "(Not yet assigned)" value ]

        _ ->
            text "other"
