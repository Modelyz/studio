module View.Smallcard exposing (clickableCard, hClickableCard, hClickableRemovableCard, hItemClickableCard, hViewHalfCard, sClickableCard, tClickableRemovableCard, tItemClickableCard, viewHalfCard, viewSmallCard)

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


hViewHalfCard : msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Element msg
hViewHalfCard onDelete allT allH configs hierarchic =
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType hierarchic.what hierarchic.uuid)

        title =
            display mconfig hierarchic
    in
    viewHalfCard (Just onDelete) (text title)


tItemClickableCard : (Scope -> msg) -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Typed a -> Type.Type -> Element msg
tItemClickableCard onInput allT allH configs typed type_ =
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (IsItem type_ typed.uuid)

        title =
            display mconfig typed

        description =
            T.find allT typed.type_ |> Maybe.map (display mconfig) |> Maybe.withDefault ""
    in
    clickableCard (onInput (IsItem type_ typed.uuid)) (text title) (text description)


hItemClickableCard : (Scope -> msg) -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Type.Type -> Element msg
hItemClickableCard onInput allT allH configs hierarchic type_ =
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (IsItem type_ hierarchic.uuid)

        title =
            display mconfig hierarchic

        description =
            hierarchic.parent |> Maybe.andThen (H.find allH) |> Maybe.map (display mconfig) |> Maybe.withDefault ""
    in
    clickableCard (onInput (IsItem type_ hierarchic.uuid)) (text title) (text description)


sClickableCard : (Scope -> msg) -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Type.Type -> Element msg
sClickableCard onInput allT allH configs hierarchic type_ =
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType hierarchic.what hierarchic.uuid)

        title =
            display mconfig hierarchic

        description =
            hierarchic.parent |> Maybe.andThen (H.find allH) |> Maybe.map (display mconfig) |> Maybe.withDefault ""
    in
    clickableCard (onInput (HasUserType type_ hierarchic.uuid)) (text title) (text description)


hClickableCard : (Maybe (Hierarchic b) -> msg) -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Element msg
hClickableCard onInput allT allH configs hierarchic =
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType hierarchic.what hierarchic.uuid)

        title =
            display mconfig hierarchic

        description =
            hierarchic.parent |> Maybe.andThen (H.find allH) |> Maybe.map (display mconfig) |> Maybe.withDefault ""
    in
    clickableCard (onInput (Just hierarchic)) (text title) (text description)


tClickableRemovableCard : msg -> msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Typed a -> Element msg
tClickableRemovableCard onChoose onDelete allT allH configs typed =
    let
        mtconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType typed.what typed.type_)

        mhconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType (Type.toHierarchic typed.what) typed.type_)

        title =
            display mtconfig typed

        description =
            H.find allH typed.type_ |> Maybe.map (display mhconfig) |> Maybe.withDefault ""
    in
    clickableRemovableCard onChoose onDelete (text title) (text description)


hClickableRemovableCard : msg -> msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Element msg
hClickableRemovableCard onChoose onDelete allT allH configs hierarchic =
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType hierarchic.what hierarchic.uuid)

        title =
            display mconfig hierarchic

        description =
            hierarchic.parent |> Maybe.andThen (H.find allH) |> Maybe.map (display mconfig) |> Maybe.withDefault ""
    in
    clickableRemovableCard onChoose onDelete (text title) (text description)
