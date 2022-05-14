module View.Wizard exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import View exposing (..)


type alias Model a b =
    { a | form : Form b }


type alias Form b =
    { b | step : Maybe Int, warning : String }


type alias FormConfig msg =
    { title : String
    , steps : List (Element msg)
    , button : Element msg
    , open : msg
    , next : Int -> msg
    , previous : Int -> msg
    , ok : msg
    , cancel : msg
    }



-- Page content that can contain a wizard with several steps, as a replacement of a one-page form


view : Model a b -> List (Element msg) -> FormConfig msg -> Element msg
view m vs c =
    let
        form =
            m.form

        nbsteps =
            List.length c.steps
    in
    if False then
        -- single page mode
        column [ width fill, alignTop, padding 20 ]
            [ column [ Border.shadow shadowStyle, padding 20, centerX, alignTop, spacing 20 ]
                (vs
                    ++ [ h2 c.title
                       , wrappedRow [ padding 20, spacing 50 ]
                            (c.steps
                                |> List.map List.singleton
                                |> List.map (row [ alignTop, width <| minimum 200 fill ])
                            )
                       , c.button
                       ]
                )
            ]

    else
        -- multi page mode
        let
            buttons s =
                [ wrappedRow [ width fill, spacing 20 ]
                    [ button.secondary
                        { onPress = Just <| c.previous nbsteps
                        , label =
                            text "← Previous"
                        }
                    , button.secondary
                        { onPress = Just <| c.cancel
                        , label =
                            text "Cancel"
                        }
                    , if s == nbsteps then
                        button.primary { onPress = Just c.ok, label = text "Validate and add this identifier" }

                      else
                        button.secondary { onPress = Just <| c.next nbsteps, label = text "Next →" }
                    , if form.warning /= "" then
                        paragraph [ Font.color color.text.warning ] [ text form.warning ]

                      else
                        none
                    ]
                ]
        in
        column [ width fill, alignTop, padding 20 ]
            [ column [ width fill, Border.shadow shadowStyle, padding 0, centerX, alignTop ]
                [ topbar c.title
                , column [ width fill, padding 20, centerX, alignTop, spacing 20 ]
                    [ Maybe.map
                        (\step ->
                            column [ width fill, alignTop, spacing 20, padding 20 ] <|
                                List.append [ el [ alignTop, alignLeft ] (h3 c.title) ] <|
                                    List.append (buttons step) <|
                                        List.reverse <|
                                            List.take 1 <|
                                                List.reverse <|
                                                    List.take step c.steps
                        )
                        form.step
                        |> Maybe.withDefault (column [ spacing 20 ] ([ button.primary { onPress = Just c.open, label = text "New..." } ] ++ vs))
                    ]
                ]
            ]
