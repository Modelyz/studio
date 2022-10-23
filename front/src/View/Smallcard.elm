module View.Smallcard exposing (clickableCard, clickableRemovableCard, hClickableCard, hClickableRemovableCard, hItemClickableCard, hViewHalfCard, sClickableCard, tClickableRemovableCard, tItemClickableCard, viewHalfCard, viewSmallCard)

import Configuration as Config exposing (Configuration)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Hierarchy.Hierarchic as H exposing (Hierarchic)
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
    row [ Background.color color.item.selected ]
        [ el [ padding 10 ] title
        , Maybe.map (\onDelete -> button.secondary onDelete "×") maybeOnDelete |> Maybe.withDefault none
        ]


hViewHalfCard : msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Element msg
hViewHalfCard onDelete allT allH configs hierarchic =
    viewHalfCard
        (Just onDelete)
        (text <| hDisplay allT allH configs SmallcardTitle hierarchic)


tItemClickableCard : (Scope -> msg) -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Typed a -> Type.Type -> Element msg
tItemClickableCard onInput allT allH configs typed type_ =
    clickableCard
        (onInput (IsItem type_ typed.uuid))
        (text <| tDisplay allT allH configs SmallcardTitle typed)
        (T.find allT typed.type_ |> Maybe.map (tDisplay allT allH configs SmallcardTitle) |> Maybe.withDefault "" |> text)


hItemClickableCard : (Scope -> msg) -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Type.Type -> Element msg
hItemClickableCard onInput allT allH configs hierarchic type_ =
    clickableCard
        (onInput (IsItem type_ hierarchic.uuid))
        (text <| hDisplay allT allH configs SmallcardTitle hierarchic)
        (hierarchic.parent |> Maybe.andThen (H.find allH) |> Maybe.map (hDisplay allT allH configs SmallcardTitle) |> Maybe.withDefault "" |> text)


sClickableCard : (Scope -> msg) -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Type.Type -> Element msg
sClickableCard onInput allT allH configs hierarchic type_ =
    clickableCard (onInput (HasUserType type_ hierarchic.uuid))
        (text <| hDisplay allT allH configs SmallcardTitle hierarchic)
        (hierarchic.parent |> Maybe.andThen (H.find allH) |> Maybe.map (hDisplay allT allH configs SmallcardTitle) |> Maybe.withDefault "" |> text)


hClickableCard : (Maybe (Hierarchic b) -> msg) -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Element msg
hClickableCard onInput allT allH configs hierarchic =
    clickableCard (onInput (Just hierarchic))
        (text <| hDisplay allT allH configs SmallcardTitle hierarchic)
        (hierarchic.parent |> Maybe.andThen (H.find allH) |> Maybe.map (hDisplay allT allH configs SmallcardTitle) |> Maybe.withDefault "" |> text)


tClickableRemovableCard : msg -> msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Typed a -> Element msg
tClickableRemovableCard onChoose onDelete allT allH configs typed =
    clickableRemovableCard onChoose
        onDelete
        (text <| tDisplay allT allH configs SmallcardTitle typed)
        (Debug.log "H.find" (H.find allH typed.type_) |> Maybe.map (hDisplay allT allH configs SmallcardTitle) |> Maybe.withDefault "" |> text)


hClickableRemovableCard : msg -> msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Element msg
hClickableRemovableCard onChoose onDelete allT allH configs hierarchic =
    clickableRemovableCard onChoose
        onDelete
        (text <| hDisplay allT allH configs SmallcardTitle hierarchic)
        (hierarchic.parent |> Maybe.andThen (H.find allH) |> Maybe.map (hDisplay allT allH configs SmallcardTitle) |> Maybe.withDefault "" |> text)
