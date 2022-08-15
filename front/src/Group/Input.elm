module Group.Input exposing (inputGroups)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Group.Group as Group exposing (Group)
import Group.View
import Prng.Uuid as Uuid exposing (Uuid)
import Shared
import View exposing (..)
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
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (el [ paddingXY 10 0, Font.size size.text.h2 ] <| text "Belongs to: ")
                :: List.map (viewItem c s model) (Dict.values model.groups)
        , h2 <| "Select the groups this entity should belong to"
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
            (s.state.groups
                |> Dict.values
                |> List.map
                    (\g ->
                        clickableCard (c.onInput <| Dict.insert (Uuid.toString g.uuid) g model.groups) (text <| Uuid.toString g.uuid) none
                     --TODO display identifiers
                    )
            )
        ]
