module View.Smallcard exposing (clickableCard, clickableRemovableCard, hClickableCard, hClickableRemovableCard, hItemClickableCard, hViewHalfCard, sClickableCard, tClickableCard, tClickableRemovableCard, tItemClickableCard, tViewHalfCard, viewHalfCard, viewSmallCard)

import Configuration as Config exposing (Configuration)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Ident.Identifier exposing (Identifier)
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope exposing (Scope(..))
import Type exposing (Type)
import Typed.Typed as T exposing (Typed)
import View exposing (..)
import View.Style exposing (..)
import Zone.View exposing (display)
import Zone.Zone exposing (Zone(..))



-- TODO merge/refactor all this


viewSmallCard : msg -> Element msg -> Element msg -> Element msg
viewSmallCard onDelete title description =
    -- TODO rename to removableCard
    column [ Background.color color.item.background ]
        [ row [ spacing 10, width fill ]
            [ row [ Font.size size.text.main, padding 10 ] [ title ]
            , el [ alignRight ] (button.primary onDelete "×")
            ]
        , row [ padding 10, Font.size size.text.small ] [ description ]
        ]


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


viewHalfCard : Maybe msg -> Element msg -> Element msg
viewHalfCard maybeOnDelete title =
    row [ Background.color color.item.selected ]
        [ el [ padding 10 ] title
        , Maybe.map (\onDelete -> button.secondary onDelete "×") maybeOnDelete |> Maybe.withDefault none
        ]


tViewHalfCard : msg -> Dict String ( Type, Maybe Uuid ) -> Dict String Configuration -> Dict String Identifier -> Type -> Uuid -> Element msg
tViewHalfCard onDelete types configs ids t uuid =
    viewHalfCard
        (Just onDelete)
        (text <| display types configs SmallcardTitle ids t uuid)


hViewHalfCard =
    tViewHalfCard


tItemClickableCard : (Scope -> msg) -> Dict String ( Type, Maybe Uuid ) -> Dict String Configuration -> Dict String Identifier -> Type -> Uuid -> Element msg
tItemClickableCard onInput types configs ids t uuid =
    clickableCard
        (onInput (IsItem t uuid))
        (text <| display types configs SmallcardTitle ids t uuid)
        (Dict.get (Uuid.toString uuid) types
            |> Maybe.andThen (\( pt, mpuuid ) -> mpuuid |> Maybe.map (\puuid -> display types configs SmallcardTitle ids pt puuid))
            |> Maybe.withDefault ""
            |> text
        )


hItemClickableCard =
    tItemClickableCard


sClickableCard : (Scope -> msg) -> Dict String ( Type, Maybe Uuid ) -> Dict String Configuration -> Dict String Identifier -> Type -> Uuid -> Scope -> Element msg
sClickableCard onInput types configs ids t uuid scope =
    clickableCard
        (onInput scope)
        (text <| display types configs SmallcardTitle ids t uuid)
        (Dict.get (Uuid.toString uuid) types
            |> Maybe.andThen (\( pt, mpuuid ) -> mpuuid |> Maybe.map (\puuid -> display types configs SmallcardTitle ids pt puuid))
            |> Maybe.withDefault ""
            |> text
        )


tClickableCard : msg -> Dict String ( Type, Maybe Uuid ) -> Dict String Configuration -> Dict String Identifier -> Type -> Uuid -> Element msg
tClickableCard onInput types configs ids t uuid =
    clickableCard
        onInput
        (text <| display types configs SmallcardTitle ids t uuid)
        (Dict.get (Uuid.toString uuid) types
            |> Maybe.andThen (\( pt, mpuuid ) -> mpuuid |> Maybe.map (\puuid -> display types configs SmallcardTitle ids pt puuid))
            |> Maybe.withDefault ""
            |> text
        )


hClickableCard =
    tClickableCard


tClickableRemovableCard : msg -> msg -> Dict String ( Type, Maybe Uuid ) -> Dict String Configuration -> Dict String Identifier -> Type -> Uuid -> Element msg
tClickableRemovableCard onChoose onDelete types configs ids t uuid =
    clickableRemovableCard
        onChoose
        onDelete
        (text <| display types configs SmallcardTitle ids t uuid)
        (Dict.get (Uuid.toString uuid) types
            |> Maybe.andThen (\( pt, mpuuid ) -> mpuuid |> Maybe.map (\puuid -> display types configs SmallcardTitle ids pt puuid))
            |> Maybe.withDefault ""
            |> text
        )


hClickableRemovableCard =
    tClickableRemovableCard
