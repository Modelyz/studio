module CommitmentType.View exposing (svg)

import Element exposing (..)
import Svg as S
import Svg.Attributes as A
import View exposing (lenToPx)
import View.Style as Style exposing (..)


svg : String -> String -> String -> Element msg
svg from what to =
    html <|
        S.svg [ A.width "800", A.height "200", A.viewBox "0 0 800 200" ]
            [ S.rect [ A.fill Style.strcolor, A.x "0", A.y "52.5", A.width "245", A.height "95", A.rx "10", A.ry "10" ] []
            , S.rect [ A.fill Style.strcolor, A.x "555", A.y "52.5", A.width "245", A.height "95", A.rx "10", A.ry "10" ] []
            , S.path [ A.fill Style.strcolor, A.x "100", A.y "10", A.width "100", A.height "70", A.d "m 440.86545,32.919698 v 31.887225 h -189.40321 v 70.388317 h 189.40321 v 31.88506 l 107.6723,-67.07922 z" ] []
            , S.text_ [ A.x "123", A.y "106", A.fill "black", A.color "black", A.fontSize <| lenToPx from, A.style "text-anchor: middle; text-align: center" ] [ S.text from ]
            , S.text_ [ A.x "389", A.y "106", A.fill "black", A.color "black", A.fontSize <| lenToPx what, A.style "text-anchor: middle; text-align: center" ] [ S.text what ]
            , S.text_ [ A.x "667", A.y "106", A.fill "black", A.color "black", A.fontSize <| lenToPx to, A.style "text-anchor: middle; text-align: center" ] [ S.text to ]
            ]
