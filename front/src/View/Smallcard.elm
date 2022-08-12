module View.Smallcard exposing (..)

import Configuration exposing (Configuration, getMostSpecific)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Hierarchy.Hierarchic exposing (Hierarchic)
import Item.Item as Item exposing (Item, OnlyItem)
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope exposing (Scope(..))
import Typed.Typed exposing (Typed)
import View exposing (..)
import View.Style exposing (..)
import Zone.Zone exposing (Zone(..))


viewSmallCard : msg -> Element msg -> Element msg -> Element msg
viewSmallCard deleteMsg title description =
    -- TODO remove and replace with newviewSmallCard
    let
        titleelm =
            row [ Font.size size.text.main, padding 10 ] [ title ]
    in
    row
        []
        [ column [ Background.color color.item.background ]
            [ row [ spacing 10, width fill ]
                [ titleelm
                , el [ alignRight ] (button.primary deleteMsg "×")
                ]
            , row [ padding 10, Font.size size.text.small ] [ description ]
            ]
        ]


hViewSmallCard : (Uuid -> msg) -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> List (Hierarchic b) -> List (Element msg)
hViewSmallCard deleteMsg allTyped allHierarchic configs =
    let
        title =
            none

        description =
            none

        titleelm =
            row [ Font.size size.text.main, padding 10 ] [ title ]
    in
    List.map
        (\item ->
            let
                mconfig =
                    getMostSpecific allTyped allHierarchic configs SmallcardTitle (item.parent |> Maybe.map (\p -> And (IsType item.what) (HasUserType p)) |> Maybe.withDefault (IsType item.what))
            in
            row
                []
                [ column [ Background.color color.item.background ]
                    [ row [ spacing 10, width fill ]
                        [ titleelm
                        , el [ alignRight ] (button.primary (deleteMsg item.uuid) "×")
                        ]
                    , row [ padding 10, Font.size size.text.small ] [ description ]
                    ]
                ]
        )
