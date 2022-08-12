module View.Smallcard exposing (..)

import Configuration as Config exposing (Configuration(..))
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Hierarchy.Hierarchic as Hierarchic exposing (Hierarchic)
import Ident.Identifiable as Identifiable exposing (Identifiable)
import Item.Item as Item exposing (Item, OnlyItem)
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope(..))
import Typed.Typed exposing (Typed)
import View exposing (..)
import View.Style exposing (..)
import Zone.Fragment exposing (displayFromDict)
import Zone.Zone exposing (Zone(..))


viewSmallCard : msg -> Element msg -> Element msg -> Element msg
viewSmallCard deleteMsg title description =
    -- TODO remove and replace with newviewSmallCard
    row
        []
        [ column [ Background.color color.item.background ]
            [ row [ spacing 10, width fill ]
                [ row [ Font.size size.text.main, padding 10 ] [ title ]
                , el [ alignRight ] (button.primary deleteMsg "×")
                ]
            , row [ padding 10, Font.size size.text.small ] [ description ]
            ]
        ]


hViewSmallCard : (Uuid -> msg) -> Dict String (Typed a) -> Dict String (Identifiable (Hierarchic b)) -> Dict String Configuration -> List (Identifiable (Hierarchic b)) -> List (Element msg)
hViewSmallCard deleteMsg allT allH configs =
    List.map
        (\h ->
            let
                mconfig =
                    Config.getMostSpecific allT allH configs SmallcardTitle (Scope.fromHierarchic h)

                title =
                    Identifiable.display mconfig h

                description =
                    h.parent |> Maybe.andThen (Hierarchic.find allH) |> Maybe.map (Identifiable.display mconfig) |> Maybe.withDefault ""
            in
            row
                []
                [ column [ Background.color color.item.background ]
                    [ row [ spacing 10, width fill ]
                        [ row [ Font.size size.text.main, padding 10 ] [ text title ]
                        , el [ alignRight ] (button.primary (deleteMsg h.uuid) "×")
                        ]
                    , row [ padding 10, Font.size size.text.small ] [ text description ]
                    ]
                ]
        )
