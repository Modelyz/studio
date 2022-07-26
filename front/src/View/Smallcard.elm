module View.Smallcard exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import View exposing (..)
import View.Style exposing (..)


viewSmallCard : msg -> Maybe String -> Element msg -> Element msg -> Element msg
viewSmallCard deleteMsg url title description =
    let
        titleelm =
            row [ Font.size size.text.main, padding 10 ] [ title ]
    in
    row
        []
        [ column [ Background.color color.item.background ]
            [ row [ spacing 10, width fill ]
                [ Maybe.withDefault titleelm <| Maybe.map (\u -> link [] { url = u, label = titleelm }) url
                , el [ alignRight ] (button.primary deleteMsg "×")
                ]
            , row [ padding 10, Font.size size.text.small ] [ description ]
            ]
        ]
