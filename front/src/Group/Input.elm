module Group.Input exposing (inputGroups)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Group.Group as Group exposing (Group)
import Group.View
import Ident.Identifiable exposing (hWithIdentifiers, tWithIdentifiers, withIdentifiers)
import Prng.Uuid as Uuid exposing (Uuid)
import Shared
import Type
import Typed.Type as TType
import View exposing (..)
import View.Smallcard exposing (tClickableCard)
import View.Style exposing (..)


type alias Model a =
    { a | groups : Dict String Group }


type alias Config msg =
    { onInput : Dict String Group -> msg
    }


viewItem : Config msg -> Shared.Model -> Model a -> Group -> Element msg
viewItem c s model group =
    -- TODO duplicated from Ident/AddPage
    row [ Background.color color.item.background ]
        [ el [ paddingXY 10 2 ] (Group.View.displayGroup s group)
        , button.primary (c.onInput <| Dict.remove (Uuid.toString group.uuid) model.groups) "×"
        ]


inputGroups : Config msg -> Shared.Model -> Model a -> Element msg
inputGroups c s model =
    -- TODO duplicated from Ident/AddPage and Zone/AddPage
    let
        allTwithIdentifiers =
            s.state.groups |> tWithIdentifiers s.state.identifiers

        allHwithIdentifiers =
            s.state.groupTypes |> hWithIdentifiers s.state.identifiers
    in
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (el [ paddingXY 10 0, Font.size size.text.h2 ] <| text "Belongs to: ")
                :: List.map (viewItem c s model) (Dict.values model.groups)
        , h2 <| "Select the groups this entity should belong to"
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
            (allTwithIdentifiers
                |> Dict.values
                |> List.map (\g -> tClickableCard (c.onInput (Dict.insert (Group.compare g) g model.groups)) allTwithIdentifiers allHwithIdentifiers s.state.configs g)
                |> withDefaultContent (p "(There are no Groups yet)")
            )
        ]
