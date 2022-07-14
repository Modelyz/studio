module Ident.Input exposing (..)

import DictSet as Set exposing (DictSet)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Ident.EntityIdentifier as EntityIdentifier exposing (EntityIdentifier)
import Ident.Fragment as Fragment exposing (Fragment(..))
import Ident.Identifiable as Identifiable exposing (Identifiable(..))
import Ident.Identifier as Identifier exposing (Identifier)
import REA.Entity exposing (Entity(..))
import Style exposing (size)
import View exposing (ViewType(..))


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
                (\i -> inputIdentifier c model i)
        )


inputIdentifier : Config msg -> Model a -> Identifier -> Element msg
inputIdentifier c model id =
    column []
        [ el [ paddingXY 0 10 ] <| text id.name
        , row [ spacing 5 ] <|
            List.indexedMap (\i f -> inputFragment c model i f id) id.fragments
        ]


inputFragment : Config msg -> Model a -> Int -> Fragment -> Identifier -> Element msg
inputFragment c model index fragment ident =
    case fragment of
        Free value ->
            Input.text
                [ width <| minimum 200 fill
                , Input.focusedOnLoad
                , View.onEnter c.onEnter
                ]
                { onChange = \v -> c.onInput <| Identifier.update index (Free v) ident
                , text = value
                , placeholder =
                    Just <| Input.placeholder [] <| text ident.name
                , label = Input.labelHidden ident.name
                }

        Fixed value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text value ]

        Sequence padding step start value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text <| Maybe.withDefault "(Not yet assigned)" value ]

        _ ->
            text "Not Implemented"
