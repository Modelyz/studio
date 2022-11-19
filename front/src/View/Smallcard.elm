module View.Smallcard exposing (clickableCard, clickableRemovableCard, hClickableCard, hClickableRemovableCard, hItemClickableCard, hViewHalfCard, sClickableCard, tClickableCard, tClickableRemovableCard, tItemClickableCard, tViewHalfCard, viewHalfCard, viewSmallCard)

import Configuration as Config exposing (Configuration)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Hierarchy.Type as HType
import Scope.Scope exposing (Scope(..))
import Type
import Typed.Typed as T exposing (Typed)
import View exposing (..)
import View.Style exposing (..)
import Zone.View exposing (hDisplay, tDisplay)
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
    -- optionnaly removable card
    row [ Background.color color.item.selected ]
        [ el [ padding 10 ] title
        , Maybe.map (\onDelete -> button.secondary onDelete "×") maybeOnDelete |> Maybe.withDefault none
        ]



-- TODO try to remove the split t/h by replacing Typed a and Hierarchic b with a and b, and passing the relevant function to extract the wanted field
-- something like:
{-
   xViewHalfCard : msg -> Dict String a -> Dict String b -> Dict String Configuration -> a -> Element msg
   xViewHalfCard onDelete allT allH configs t =
       viewHalfCard
           (Just onDelete)
           (text <| tDisplay allT allH configs SmallcardTitle t)
-}


tViewHalfCard : Maybe msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Typed a -> Element msg
tViewHalfCard onDelete allT allH configs t =
    -- optionnaly removable card
    viewHalfCard
        onDelete
        (text <| tDisplay allT allH configs SmallcardTitle t)


hViewHalfCard : Maybe msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Element msg
hViewHalfCard onDelete allT allH configs h =
    -- optionnaly removable card
    viewHalfCard
        onDelete
        (text <| hDisplay allT allH configs SmallcardTitle h)


tItemClickableCard : (Scope -> msg) -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Typed a -> Type.Type -> Element msg
tItemClickableCard onInput allT allH configs t type_ =
    clickableCard
        (onInput (IsItem type_ t.uuid))
        (text <| tDisplay allT allH configs SmallcardTitle t)
        (T.find allT t.type_ |> Maybe.map (tDisplay allT allH configs SmallcardTitle) |> Maybe.withDefault "" |> text)


hItemClickableCard : (Scope -> msg) -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Type.Type -> Element msg
hItemClickableCard onInput allT allH configs h type_ =
    clickableCard
        (onInput (IsItem type_ h.uuid))
        (text <| hDisplay allT allH configs SmallcardTitle h)
        (h.parent |> Maybe.andThen (H.find allH) |> Maybe.map (hDisplay allT allH configs SmallcardTitle) |> Maybe.withDefault "" |> text)


sClickableCard : (Scope -> msg) -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Scope -> Element msg
sClickableCard onInput allT allH configs h scope =
    clickableCard
        (onInput scope)
        (text <| hDisplay allT allH configs SmallcardTitle h)
        (h.parent |> Maybe.andThen (H.find allH) |> Maybe.map (hDisplay allT allH configs SmallcardTitle) |> Maybe.withDefault "" |> text)


tClickableCard : msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Typed a -> Element msg
tClickableCard onInput allT allH configs t =
    clickableCard
        onInput
        (text <| tDisplay allT allH configs SmallcardTitle t)
        (H.find allH t.type_ |> Maybe.map (hDisplay allT allH configs SmallcardTitle) |> Maybe.withDefault "" |> text)


hClickableCard : msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Element msg
hClickableCard onInput allT allH configs h =
    clickableCard
        onInput
        (text <| hDisplay allT allH configs SmallcardTitle h)
        (h.parent |> Maybe.andThen (H.find allH) |> Maybe.map (hDisplay allT allH configs SmallcardTitle) |> Maybe.withDefault "" |> text)


tClickableRemovableCard : msg -> msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Typed a -> Element msg
tClickableRemovableCard onChoose onDelete allT allH configs t =
    clickableRemovableCard
        onChoose
        onDelete
        (text <| tDisplay allT allH configs SmallcardTitle t)
        (H.find allH t.type_ |> Maybe.map (hDisplay allT allH configs SmallcardTitle) |> Maybe.withDefault "" |> text)


hClickableRemovableCard : msg -> msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Element msg
hClickableRemovableCard onChoose onDelete allT allH configs h =
    clickableRemovableCard
        onChoose
        onDelete
        (text <| hDisplay allT allH configs SmallcardTitle h)
        (h.parent |> Maybe.andThen (H.find allH) |> Maybe.map (hDisplay allT allH configs SmallcardTitle) |> Maybe.withDefault "" |> text)
