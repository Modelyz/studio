module Tree.View exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Tree
import View exposing (..)
import View.Style exposing (color)


inputTreeType : (Tree.TreeType -> msg) -> Tree.TreeType -> Element msg
inputTreeType onInput treetype =
    column [ spacing 5, width fill ] <|
        List.map
            (\t ->
                wrappedRow [ spacing 20 ]
                    [ el
                        [ pointer
                        , padding 10
                        , width fill
                        , Background.color <|
                            if t == treetype then
                                color.item.selected

                            else
                                color.item.background
                        , onClick (onInput t)
                        ]
                        (p <| Tree.toString t)
                    ]
            )
            Tree.all
