module View.TagList exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Style exposing (..)
import View exposing (button, h2)


type alias Model m a =
    { m | taglist : List a }


type alias TagStrings =
    -- different representations of a Fragment
    { type_ : String, name : String, value : String, desc : String }


type alias Config a msg =
    { all : List a
    , toString : a -> String
    , toDesc : a -> String
    , inputmsg : List a -> msg
    , label : String
    , explain : Element msg
    }


taglist : Model m a -> Config a msg -> Element msg
taglist model c =
    column [ alignTop, spacing 10, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
            h2 c.label
                :: List.append
                    (if List.isEmpty model.taglist then
                        [ el [ padding 5, Font.color color.text.disabled ] (text "Empty") ]

                     else
                        []
                    )
                    (List.indexedMap
                        (\i p ->
                            row [ Background.color color.item.selected ]
                                [ el [ padding 5 ] (text <| c.toString p)
                                , button.secondary
                                    (c.inputmsg
                                        (model.taglist
                                            |> List.indexedMap Tuple.pair
                                            |> List.filter (\( j, _ ) -> j /= i)
                                            |> List.map Tuple.second
                                        )
                                    )
                                    "×"
                                ]
                        )
                        model.taglist
                    )
        , c.explain
        , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ] <|
            List.map
                (\p ->
                    column [ Background.color color.item.background, mouseOver itemHoverstyle, width (px 250), height (px 150) ]
                        [ row [ alignLeft ]
                            [ button.primary (c.inputmsg <| model.taglist ++ [ p ]) "+"
                            , el [ paddingXY 10 0 ] (text <| c.toString p)
                            ]
                        , paragraph [ padding 10, Font.size size.text.main ] [ text <| c.toDesc p ]
                        ]
                )
                c.all
        ]
