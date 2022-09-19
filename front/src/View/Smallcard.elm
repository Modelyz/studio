module View.Smallcard exposing (..)

import Configuration as Config exposing (Configuration(..))
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events exposing (onClick)
import Element.Font as Font
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Ident.Identifiable as Identifiable exposing (Identifiable)
import Item.Item as Item exposing (Item)
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope(..))
import Type as Type
import Typed.Type as TType
import Typed.Typed as T exposing (Typed)
import View exposing (..)
import View.Style exposing (..)
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


tViewHalfCard : msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Typed a -> Element msg
tViewHalfCard onDelete allT allH configs h =
    -- smallcard for typed items
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType h.what h.uuid)

        title =
            Identifiable.display mconfig h
    in
    viewHalfCard onDelete (text title)


hViewHalfCard : msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Element msg
hViewHalfCard onDelete allT allH configs h =
    -- smallcard for hierarchic items
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType h.what h.uuid)

        title =
            Identifiable.display mconfig h
    in
    viewHalfCard onDelete (text title)


hViewSmallCard : msg -> Dict String (Typed a) -> Dict String (Identifiable (Hierarchic b)) -> Dict String Configuration -> Identifiable (Hierarchic b) -> Element msg
hViewSmallCard onDelete allT allH configs h =
    -- smallcard for hierarchic items
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType h.what h.uuid)

        title =
            Identifiable.display mconfig h

        description =
            h.parent |> Maybe.andThen (H.find allH) |> Maybe.map (Identifiable.display mconfig) |> Maybe.withDefault ""
    in
    viewSmallCard onDelete (text title) (text description)


tClickableCard : msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Typed a -> Element msg
tClickableCard onInput allT allH configs t =
    -- clickable card for typed items
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType t.what t.uuid)

        title =
            Identifiable.display mconfig t

        description =
            H.find allH t.type_ |> Maybe.map (Identifiable.display mconfig) |> Maybe.withDefault ""
    in
    clickableCard onInput (text title) (text description)


sClickableCard : (Scope -> msg) -> Dict String (Typed a) -> Dict String (Identifiable (Hierarchic b)) -> Dict String Configuration -> Identifiable (Hierarchic b) -> Type.Type -> Element msg
sClickableCard onInput allT allH configs h t =
    -- clickable card for typed items
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType h.what h.uuid)

        title =
            Identifiable.display mconfig h

        description =
            h.parent |> Maybe.andThen (H.find allH) |> Maybe.map (Identifiable.display mconfig) |> Maybe.withDefault ""
    in
    clickableCard (onInput (HasUserType t h.uuid)) (text title) (text description)


hClickableCard : (Maybe (Identifiable (Hierarchic b)) -> msg) -> Dict String (Typed a) -> Dict String (Identifiable (Hierarchic b)) -> Dict String Configuration -> Identifiable (Hierarchic b) -> Element msg
hClickableCard onInput allT allH configs h =
    -- clickable card for typed items
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType h.what h.uuid)

        title =
            Identifiable.display mconfig h

        description =
            h.parent |> Maybe.andThen (H.find allH) |> Maybe.map (Identifiable.display mconfig) |> Maybe.withDefault ""
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
            Identifiable.display mtconfig t

        description =
            H.find allH t.type_ |> Maybe.map (Identifiable.display mhconfig) |> Maybe.withDefault ""
    in
    clickableRemovableCard onChoose onDelete (text title) (text description)


hClickableRemovableCard : msg -> msg -> Dict String (Typed a) -> Dict String (Hierarchic b) -> Dict String Configuration -> Hierarchic b -> Element msg
hClickableRemovableCard onChoose onDelete allT allH configs h =
    -- clickable card for typed items
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType h.what h.uuid)

        title =
            Identifiable.display mconfig h

        description =
            h.parent |> Maybe.andThen (H.find allH) |> Maybe.map (Identifiable.display mconfig) |> Maybe.withDefault ""
    in
    clickableRemovableCard onChoose onDelete (text title) (text description)


tViewSmallCard : msg -> Dict String (Typed a) -> Dict String (Identifiable (Hierarchic b)) -> Dict String Configuration -> Identifiable (Typed a) -> Element msg
tViewSmallCard onDelete allT allH configs t =
    -- smallcard for hierarchic items
    let
        mtconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType t.what t.type_)

        mhconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (HasUserType (Type.toHierarchic t.what) t.type_)

        title =
            Identifiable.display mtconfig t

        description =
            H.find allH t.type_ |> Maybe.map (Identifiable.display mhconfig) |> Maybe.withDefault ""
    in
    viewSmallCard onDelete (text title) (text description)
