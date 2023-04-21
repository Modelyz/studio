module View.MultiSelect exposing (multiSelect)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import View exposing (..)
import View.Smallcard exposing (clickableCard)
import View.Style exposing (..)


type alias Config model item msg =
    { inputMsg : List item -> msg
    , selection : model -> List item
    , title : String
    , description : String
    , empty : String
    , toString : item -> String
    , toDesc : item -> String
    , height : Int

    -- edit the Nth item of the list
    , input : List item -> Int -> item -> Element msg
    }


multiSelect : model -> Config model item msg -> List item -> Element msg
multiSelect model c all =
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (el [ padding 5, spacing 10, width fill ] <| h2 c.title)
                :: List.append
                    (if List.isEmpty (c.selection model) then
                        [ el [ padding 10, Font.color color.text.disabled ] (text "Empty") ]

                     else
                        []
                    )
                    (c.selection model
                        |> List.indexedMap
                            (\i item ->
                                row [ paddingEach { zero | right = 5 }, spacing 5, Background.color color.item.background ]
                                    [ button.primary
                                        (Ok <|
                                            c.inputMsg
                                                (c.selection model
                                                    |> List.indexedMap Tuple.pair
                                                    |> List.filter (\( j, _ ) -> j /= i)
                                                    |> List.map Tuple.second
                                                )
                                        )
                                        "×"
                                    , text <| c.toString item ++ ":"
                                    , c.input (c.selection model) i item
                                    ]
                            )
                    )
        , h2 c.description
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ] <|
            withDefaultContent (p c.empty) <|
                List.map
                    (\item ->
                        clickableCard
                            (c.inputMsg <| c.selection model ++ [ item ])
                            (text <| c.toString item)
                            (paragraph [ Font.size size.text.small, height (px c.height) ] [ text <| c.toDesc item ])
                    )
                    all
        ]
