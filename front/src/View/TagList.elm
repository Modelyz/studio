module View.TagList exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import REA.Ident exposing (fragmentToName)
import Style exposing (..)
import View exposing (button, h2)


type alias Model m a =
    { m | tags : List a }


type alias TagStrings =
    -- different representations of a Fragment
    { type_ : String, name : String, value : String, desc : String }


type alias Config a msg =
    { all : List a
    , toString : a -> String
    , toDesc : a -> String
    , onInput : List a -> msg
    , toName : a -> Maybe String
    , setName : String -> a -> a
    , label : String
    , explain : Element msg
    }


inputTags : Model m a -> Config a msg -> Element msg
inputTags model c =
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (el [ paddingXY 10 0 ] <| h2 c.label)
                :: List.append
                    (if List.isEmpty model.tags then
                        [ el [ padding 10, Font.color color.text.disabled ] (text "Empty") ]

                     else
                        []
                    )
                    (model.tags
                        |> List.indexedMap
                            (\i tag ->
                                row [ Background.color color.item.background ]
                                    [ el [ paddingXY 10 2 ] (text <| c.toString tag)
                                    , c.toName tag
                                        |> Maybe.map
                                            (\tagname ->
                                                Input.text [ width (px 75) ]
                                                    { onChange =
                                                        \v ->
                                                            c.onInput
                                                                (model.tags
                                                                    |> List.indexedMap
                                                                        (\index t ->
                                                                            if index == i then
                                                                                c.setName v t

                                                                            else
                                                                                t
                                                                        )
                                                                )
                                                    , text = tagname
                                                    , placeholder =
                                                        Just <| Input.placeholder [] <| text <| c.toString tag
                                                    , label = Input.labelHidden <| tagname
                                                    }
                                            )
                                        |> Maybe.withDefault none
                                    , button.primary
                                        (c.onInput
                                            (model.tags
                                                |> List.indexedMap Tuple.pair
                                                |> List.filter (\( j, _ ) -> j /= i)
                                                |> List.map Tuple.second
                                            )
                                        )
                                        "×"
                                    ]
                            )
                    )
        , c.explain
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ] <|
            List.map
                (\p ->
                    column
                        [ Background.color color.item.background
                        , mouseOver itemHoverstyle
                        , width (px 250)
                        , onClick (c.onInput <| model.tags ++ [ p ])
                        , pointer
                        , padding 10
                        , spacing 10
                        , height (px 150)
                        ]
                        [ el [] (text <| c.toString p)
                        , paragraph [ Font.size size.text.main ] [ text <| c.toDesc p ]
                        ]
                )
                c.all
        ]
