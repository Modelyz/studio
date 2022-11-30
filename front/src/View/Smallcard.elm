module View.Smallcard exposing (clickableCard, clickableRemovableCard, hItemClickableCard, halfCard, tClickableCard, tClickableRemovableCard, viewHalfCard)

import Configuration exposing (Configuration)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Group.Group exposing (Group)
import Group.Link exposing (Link)
import Ident.Identifier as Identifier exposing (Identifier)
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope exposing (Scope(..))
import Type exposing (Type)
import Util exposing (third)
import View exposing (..)
import View.Style exposing (..)
import Zone.View
import Zone.Zone exposing (Zone(..))



-- TODO merge/refactor all this


clickableRemovableCard : msg -> msg -> Element msg -> Element msg -> Element msg
clickableRemovableCard onChoose onDelete title description =
    column [ Background.color color.item.background ]
        [ row [ spacing 10, width fill ]
            [ row [ Font.size size.text.main, padding 10, onClick onChoose, pointer ] [ title ]
            , el [ alignRight ] (button.primary onDelete "×")
            ]
        , row [ padding 10, Font.size size.text.small ] [ description ]
        ]


clickableCard : msg -> Element msg -> Element msg -> Element msg
clickableCard onInput title description =
    column [ pointer, onClick onInput, Background.color color.item.background, mouseOver itemHoverstyle, height (px 75) ]
        [ row [ alignLeft, Font.size size.text.main ]
            [ button.primary onInput "+"
            , el [ paddingXY 10 0 ] title
            ]
        , row [ padding 10, Font.size size.text.small ] [ description ]
        ]


halfCard : msg -> Element msg -> Element msg
halfCard onDelete title =
    row [ Background.color color.item.selected ] [ el [ padding 10 ] title, button.secondary onDelete "×" ]


viewHalfCard : Maybe msg -> Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String Configuration -> Dict String Identifier -> Dict String Link -> Type -> Uuid -> Element msg
viewHalfCard maybeOnDelete types configs ids grouplinks t uuid =
    row [ Background.color color.item.selected ]
        [ el [ padding 10 ] (text <| Zone.View.display types configs SmallcardTitle ids grouplinks t uuid)
        , Maybe.map (\onDelete -> button.secondary onDelete "×") maybeOnDelete |> Maybe.withDefault none
        ]


tItemClickableCard : (Scope -> msg) -> Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String Configuration -> Dict String Identifier -> Dict String Link -> Type -> Uuid -> Element msg
tItemClickableCard onInput types configs ids grouplinks t uuid =
    clickableCard
        (onInput (IsItem t uuid))
        (text <| Zone.View.display types configs SmallcardTitle ids grouplinks t uuid)
        (Dict.get (Uuid.toString uuid) types
            |> Maybe.andThen third
            |> Maybe.map (\puuid -> Zone.View.display types configs SmallcardTitle (Identifier.fromUuid puuid ids) grouplinks (Type.toHierarchic t) puuid)
            |> Maybe.withDefault ""
            |> text
        )


hItemClickableCard =
    tItemClickableCard


tClickableCard : msg -> Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String Configuration -> Dict String Identifier -> Dict String Link -> Type -> Uuid -> Element msg
tClickableCard onInput types configs ids grouplinks t uuid =
    clickableCard
        onInput
        (text <| Zone.View.display types configs SmallcardTitle ids grouplinks t uuid)
        (Dict.get (Uuid.toString uuid) types
            |> Maybe.andThen third
            |> Maybe.map (\puuid -> Zone.View.display types configs SmallcardTitle (Identifier.fromUuid puuid ids) grouplinks (Type.toHierarchic t) puuid)
            |> Maybe.withDefault ""
            |> text
        )


tClickableRemovableCard : msg -> msg -> Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String Configuration -> Dict String Identifier -> Dict String Link -> Type -> Uuid -> Element msg
tClickableRemovableCard onChoose onDelete types configs ids grouplinks t uuid =
    clickableRemovableCard
        onChoose
        onDelete
        (text <| Zone.View.display types configs SmallcardTitle ids grouplinks t uuid)
        (Dict.get (Uuid.toString uuid) types
            |> Maybe.andThen third
            |> Maybe.map (\puuid -> Zone.View.display types configs SmallcardTitle (Identifier.fromUuid puuid ids) grouplinks (Type.toHierarchic t) puuid)
            |> Maybe.withDefault ""
            |> text
        )
