module Ident.Input exposing (Config, Model, inputIdentifiers)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Input as Input
import Ident.Fragment exposing (Fragment(..))
import Ident.Identifier as Identifier exposing (Identifier)
import Scope.Scope as Scope exposing (Scope)
import View exposing (..)


type alias Model a =
    { a | identifiers : Dict String Identifier }


type alias Config msg =
    { onEnter : msg
    , onInput : Identifier -> msg
    }


inputIdentifiers : Config msg -> Model a -> Scope -> Element msg
inputIdentifiers c model scope =
    -- display an input field for each relevant identifier
    column [ spacing 10 ]
        (h2 "Input identifiers"
            :: (model.identifiers
                    |> Dict.values
                    |> List.map
                        (\i -> inputIdentifier c model i)
                    |> withDefaultContent (p <| "Apparently there are no identifiers defined for " ++ Scope.toString scope ++ ". Please first create one.")
                --TODO + link
               )
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

        Sequence _ _ _ value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text <| Maybe.withDefault "(Not yet assigned)" value ]

        _ ->
            text "Not Implemented"
