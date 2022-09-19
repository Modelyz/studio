module Group.Input exposing (inputGroups)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Group.Group as Group exposing (Group)
import Group.View
import Ident.Identifiable as Identifiable exposing (hWithIdentifiers, tWithIdentifiers, withIdentifiers)
import Prng.Uuid as Uuid exposing (Uuid)
import Shared
import Type
import Typed.Type as TType
import View exposing (..)
import View.Smallcard exposing (clickableCard, viewHalfCard)
import View.Style exposing (..)
import Zone.View exposing (display, hWithDisplay, tWithDisplay)
import Zone.Zone exposing (Zone(..))


type alias Model a =
    { a | uuid : Uuid, groups : Dict String Group }


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
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (el [ paddingXY 10 0, Font.size size.text.h2 ] <| text "Belongs to: ")
                :: (model.groups
                        |> Dict.values
                        |> List.map (withIdentifiers s.state.identifiers)
                        |> List.map (tWithDisplay s.state.groups s.state.groupTypes s.state.configs SmallcardTitle)
                        |> List.map
                            (\t ->
                                viewHalfCard (c.onInput <| Dict.remove (Uuid.toString model.uuid) model.groups) (t.display |> Dict.get "SmallcardTitle" |> Maybe.withDefault "(missing zone config)" |> text)
                            )
                   )
        , h2 <| "Select the groups this entity should belong to"
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
            (s.state.groups
                |> Dict.values
                |> List.map (withIdentifiers s.state.identifiers)
                |> List.map (tWithDisplay s.state.groups s.state.groupTypes s.state.configs SmallcardTitle)
                |> List.map
                    (\t ->
                        clickableCard (c.onInput <| Dict.insert (Uuid.toString t.uuid) t model.groups)
                            (t.display |> Dict.get "SmallcardTitle" |> Maybe.withDefault "(missing zone config)" |> text)
                            none
                    )
                |> withDefaultContent (p "(There are no Groups yet)")
            )
        ]
