module Ident.Input exposing (Config, inputIdentifiers)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Input as Input
import Html.Attributes as Attr
import Ident.Fragment exposing (Fragment(..))
import Ident.Identifier as Identifier exposing (Identifier)
import View exposing (..)


type alias Config msg =
    { onEnter : msg
    , onInput : Identifier -> msg
    }


inputIdentifiers : Config msg -> Dict String Identifier -> Element msg
inputIdentifiers c identifiers =
    -- display an input field for each relevant identifier
    column [ spacing 10 ]
        (h2 "Identifiers"
            :: (identifiers
                    |> Dict.values
                    |> List.map
                        (\i -> inputIdentifier c i)
                    |> withDefaultContent (p <| "Apparently there are no identifiers defined for this entity. Please first create one.")
                --TODO + link
               )
        )


inputIdentifier : Config msg -> Identifier -> Element msg
inputIdentifier c id =
    column []
        [ el [ paddingXY 0 10 ] <| text (id.name ++ " :")
        , row [ spacing 5 ] <|
            List.indexedMap (\i f -> inputFragment c i f id) id.fragments
        ]


inputFragment : Config msg -> Int -> Fragment -> Identifier -> Element msg
inputFragment c index fragment ident =
    case fragment of
        Free value ->
            Input.text
                [ width <| minimum 200 fill
                , Input.focusedOnLoad
                , View.onEnter c.onEnter
                , htmlAttribute <| Attr.id (ident.name ++ "/" ++ String.fromInt index)
                ]
                { onChange = \v -> c.onInput <| Identifier.update index (Free v) ident
                , text = value
                , placeholder =
                    Just <| Input.placeholder [] <| text ident.name
                , label = Input.labelHidden ident.name
                }

        Fixed value ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text value ]

        Sequence s ->
            row [ width <| minimum 20 fill, height (px 30) ] [ text <| Maybe.withDefault "(Not yet assigned)" <| Maybe.map String.fromInt s.val ]

        _ ->
            text "Not Implemented"
