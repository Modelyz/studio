module View.Smallcard exposing (..)

import Configuration as Config exposing (Configuration(..))
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events exposing (onClick)
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
    column [ Background.color color.item.background ]
        [ row [ spacing 10, width fill ]
            [ row [ Font.size size.text.main, padding 10 ] [ title ]
            , el [ alignRight ] (button.primary deleteMsg "×")
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
viewHalfCard deleteMsg title =
    row [ Background.color color.item.selected ]
        [ el [ padding 10 ] title
        , button.secondary deleteMsg "×"
        ]


hViewHalfCard : msg -> Dict String (Typed a) -> Dict String (Identifiable (Hierarchic b)) -> Dict String Configuration -> Identifiable (Hierarchic b) -> Element msg
hViewHalfCard deleteMsg allT allH configs h =
    -- smallcard for hierarchic items
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (Scope.fromHierarchic h)

        title =
            Identifiable.display mconfig h

        description =
            h.parent |> Maybe.andThen (Hierarchic.find allH) |> Maybe.map (Identifiable.display mconfig) |> Maybe.withDefault ""
    in
    viewHalfCard deleteMsg (text title)


hViewSmallCard : msg -> Dict String (Typed a) -> Dict String (Identifiable (Hierarchic b)) -> Dict String Configuration -> Identifiable (Hierarchic b) -> Element msg
hViewSmallCard deleteMsg allT allH configs h =
    -- smallcard for hierarchic items
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (Scope.fromHierarchic h)

        title =
            Identifiable.display mconfig h

        description =
            h.parent |> Maybe.andThen (Hierarchic.find allH) |> Maybe.map (Identifiable.display mconfig) |> Maybe.withDefault ""
    in
    viewSmallCard deleteMsg (text title) (text description)


hClickableCard : (Maybe (Identifiable (Hierarchic b)) -> msg) -> Dict String (Typed a) -> Dict String (Identifiable (Hierarchic b)) -> Dict String Configuration -> Identifiable (Hierarchic b) -> Element msg
hClickableCard onInput allT allH configs h =
    -- clickable card for typed items
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (Scope.fromHierarchic h)

        title =
            Identifiable.display mconfig h

        description =
            h.parent |> Maybe.andThen (Hierarchic.find allH) |> Maybe.map (Identifiable.display mconfig) |> Maybe.withDefault ""
    in
    clickableCard (onInput (Just h)) (text title) (text description)


tViewHalfCard : msg -> Dict String (Typed a) -> Dict String (Identifiable (Hierarchic b)) -> Dict String Configuration -> Identifiable (Typed b) -> Element msg
tViewHalfCard deleteMsg allT allH configs t =
    -- smallcard for hierarchic items
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (Scope.fromTyped t)

        title =
            Identifiable.display mconfig t

        description =
            Item.find allH t.type_ |> Maybe.map (Identifiable.display mconfig) |> Maybe.withDefault ""
    in
    viewHalfCard deleteMsg (text title)


tViewSmallCard : msg -> Dict String (Typed a) -> Dict String (Identifiable (Hierarchic b)) -> Dict String Configuration -> Identifiable (Typed b) -> Element msg
tViewSmallCard deleteMsg allT allH configs t =
    -- smallcard for hierarchic items
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (Scope.fromTyped t)

        title =
            Identifiable.display mconfig t

        description =
            Item.find allH t.type_ |> Maybe.map (Identifiable.display mconfig) |> Maybe.withDefault ""
    in
    viewSmallCard deleteMsg (text title) (text description)


tClickableCard : (Maybe (Identifiable (Typed b)) -> msg) -> Dict String (Typed a) -> Dict String (Identifiable (Hierarchic b)) -> Dict String Configuration -> Identifiable (Typed b) -> Element msg
tClickableCard onInput allT allH configs t =
    -- clickable card for typed items
    let
        mconfig =
            Config.getMostSpecific allT allH configs SmallcardTitle (Scope.fromTyped t)

        title =
            Identifiable.display mconfig t

        description =
            Item.find allH t.type_ |> Maybe.map (Identifiable.display mconfig) |> Maybe.withDefault ""
    in
    clickableCard (onInput (Just t)) (text title) (text description)
