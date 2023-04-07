module View.Smallcard exposing (clickableCard, clickableRemovableCard, halfCard, itemClickableCard, tClickableCard, tClickableRemovableCard, viewHalfCard)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Prng.Uuid as Uuid exposing (Uuid)
import Scope exposing (Scope(..))
import State exposing (State)
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
            , el [ alignRight ] (button.primary (Ok onDelete) "×")
            ]
        , row [ padding 10, Font.size size.text.small ] [ description ]
        ]


clickableCard : msg -> Element msg -> Element msg -> Element msg
clickableCard onInput title description =
    column [ pointer, onClick onInput, Background.color color.item.background, mouseOver itemHoverstyle, width (px 250) ]
        [ row [ alignLeft, Font.size size.text.main ]
            [ button.primary (Ok onInput) "+"
            , el [ paddingXY 10 0 ] title
            ]
        , row [ padding 10, Font.size size.text.small ] [ description ]
        ]


halfCard : msg -> Element msg -> Element msg
halfCard onDelete title =
    row [ Background.color color.item.selected ] [ el [ padding 10 ] title, button.secondary (Ok onDelete) "×" ]


viewHalfCard : State -> Maybe msg -> Type -> Uuid -> Element msg
viewHalfCard s maybeOnDelete t uuid =
    row [ Background.color color.item.selected ]
        [ el [ padding 10 ] (text <| Zone.View.displayZone s SmallcardTitle t uuid)
        , Maybe.map (\onDelete -> button.secondary (Ok onDelete) "×") maybeOnDelete |> Maybe.withDefault none
        ]


itemClickableCard : State -> (Scope -> msg) -> Type -> Uuid -> Element msg
itemClickableCard s onInput t uuid =
    clickableCard
        (onInput (IsItem t uuid))
        (text <| Zone.View.displayZone s SmallcardTitle t uuid)
        (Dict.get (Uuid.toString uuid) s.types
            |> Maybe.andThen third
            |> Maybe.map (\puuid -> Zone.View.displayZone s SmallcardTitle {- TODO check? (Identifier.fromUuid puuid ids) -} (Type.toHierarchic t) puuid)
            |> Maybe.withDefault ""
            |> text
        )


tClickableCard : State -> msg -> Type -> Uuid -> Element msg
tClickableCard s onInput t uuid =
    clickableCard
        onInput
        (text <| Zone.View.displayZone s SmallcardTitle t uuid)
        (Dict.get (Uuid.toString uuid) s.types
            |> Maybe.andThen third
            |> Maybe.map (\puuid -> Zone.View.displayZone s SmallcardTitle {- TODO check?(Identifier.fromUuid puuid ids) -} (Type.toHierarchic t) puuid)
            |> Maybe.withDefault ""
            |> text
        )


tClickableRemovableCard : State -> msg -> msg -> Type -> Uuid -> Element msg
tClickableRemovableCard s onChoose onDelete t uuid =
    clickableRemovableCard
        onChoose
        onDelete
        (text <| Zone.View.displayZone s SmallcardTitle t uuid)
        (Dict.get (Uuid.toString uuid) s.types
            |> Maybe.andThen third
            |> Maybe.map (\puuid -> Zone.View.displayZone s SmallcardTitle {- TODO check?(Identifier.fromUuid puuid ids) -} (Type.toHierarchic t) puuid)
            |> Maybe.withDefault ""
            |> text
        )
