module View.MultiSelect exposing (multiSelect)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import View exposing (..)
import View.Smallcard exposing (clickableCard)
import View.Style exposing (..)


type alias Model model fragment =
    { model | fragments : List fragment }


type alias Config =
    { inputFragments : List fragment -> msg
    , toString : fragment -> String
    , toDesc : fragment -> String
    , inputFragment : List fragment -> Int -> fragment -> Element msg
    }


multiSelect : Model model fragment -> Config -> List fragment -> Element msg
multiSelect model c all =
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (el [ padding 5, spacing 10 ] <| h2 "Format: ")
                :: List.append
                    (if List.isEmpty model.fragments then
                        [ el [ padding 10, Font.color color.text.disabled ] (text "Empty") ]

                     else
                        []
                    )
                    (model.fragments
                        |> List.indexedMap
                            (\i fragment ->
                                row [ paddingEach { zero | right = 5 }, spacing 5, Background.color color.item.background ]
                                    [ button.primary
                                        (c.inputFragments
                                            (model.fragments
                                                |> List.indexedMap Tuple.pair
                                                |> List.filter (\( j, _ ) -> j /= i)
                                                |> List.map Tuple.second
                                            )
                                        )
                                        "×"
                                    , text <| c.toString fragment
                                    , c.inputFragment model.fragments i fragment
                                    ]
                            )
                    )
        , h2 "Click on the items below to construct the format of your identifier"
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ] <|
            List.map
                (\f ->
                    clickableCard
                        (c.inputFragments <| model.fragments ++ [ f ])
                        (text <| c.toDesc f)
                        (text <| c.toString f)
                )
                all
        ]
