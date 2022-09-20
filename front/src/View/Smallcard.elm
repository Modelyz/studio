module View.Smallcard exposing (clickableCard, hClickableCard, hClickableRemovableCard, hViewHalfCard, sClickableCard, tClickableRemovableCard, viewHalfCard, viewSmallCard)

import Configuration as Config exposing (Configuration)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Ident.Identifiable exposing (Identifiable)
import Scope.Scope exposing (Scope(..))
import Type
import Typed.Typed exposing (Typed)
import View exposing (..)
import View.Style exposing (..)
import Zone.View exposing (display)
import Zone.Zone exposing (Zone(..))



-- TODO merge all this


viewSmallCard : msg -> Element msg -> Element msg -> Element msg
viewSmallCard onDelete title description =
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


viewHalfCard : msg -> Element msg -> Element msg
viewHalfCard onDelete title =
    row [ Background.color color.item.selected ]
        [ el [ padding 10 ] title
        , button.secondary onDelete "×"
        ]


hViewHalfCard : msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Element msg
hViewHalfCard onDelete allT allH configs h =
    -- smallcard for hierarchic items
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType h.what h.uuid)

        title =
            display mconfig h
    in
    viewHalfCard onDelete (text title)


sClickableCard : (Scope -> msg) -> Dict String (Typed a) -> Dict String (Identifiable (Hierarchic b)) -> Dict String Configuration -> Identifiable (Hierarchic b) -> Type.Type -> Element msg
sClickableCard onInput allT allH configs h t =
    -- clickable card for typed items
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType h.what h.uuid)

        title =
            display mconfig h

        description =
            h.parent |> Maybe.andThen (H.find allH) |> Maybe.map (display mconfig) |> Maybe.withDefault ""
    in
    clickableCard (onInput (HasUserType t h.uuid)) (text title) (text description)


hClickableCard : (Maybe (Identifiable (Hierarchic b)) -> msg) -> Dict String (Typed a) -> Dict String (Identifiable (Hierarchic b)) -> Dict String Configuration -> Identifiable (Hierarchic b) -> Element msg
hClickableCard onInput allT allH configs h =
    -- clickable card for typed items
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType h.what h.uuid)

        title =
            display mconfig h

        description =
            h.parent |> Maybe.andThen (H.find allH) |> Maybe.map (display mconfig) |> Maybe.withDefault ""
    in
    clickableCard (onInput (Just h)) (text title) (text description)


tClickableRemovableCard : msg -> msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Typed a -> Element msg
tClickableRemovableCard onChoose onDelete allT allH configs t =
    -- clickable card for typed items
    let
        mtconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType t.what t.type_)

        mhconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType (Type.toHierarchic t.what) t.type_)

        title =
            display mtconfig t

        description =
            H.find allH t.type_ |> Maybe.map (display mhconfig) |> Maybe.withDefault ""
    in
    clickableRemovableCard onChoose onDelete (text title) (text description)


hClickableRemovableCard : msg -> msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Element msg
hClickableRemovableCard onChoose onDelete allT allH configs h =
    -- clickable card for typed items
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType h.what h.uuid)

        title =
            display mconfig h

        description =
            h.parent |> Maybe.andThen (H.find allH) |> Maybe.map (display mconfig) |> Maybe.withDefault ""
    in
    clickableRemovableCard onChoose onDelete (text title) (text description)
