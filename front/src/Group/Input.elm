module Group.Input exposing (Config, inputGroups)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Shared
import Type exposing (Type)
import Typed.Type as TType
import View exposing (..)
import View.Smallcard exposing (tClickableCard, viewHalfCard)
import View.Style exposing (..)


type alias Config msg =
    { onInput : Dict String Uuid -> msg
    , type_ : Type
    , mpuuid : Maybe Uuid
    }


inputGroups : Config msg -> Shared.Model -> Dict String Uuid -> Element msg
inputGroups c s uuids =
    -- TODO duplicated from Ident/AddPage and Zone/AddPage
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 10, spacing 5, Border.color color.item.border ] <|
            (el [ paddingXY 10 0, Font.size size.text.h2 ] <| text "Belongs to: ")
                :: (uuids
                        |> Dict.values
                        |> List.map
                            (\uuid ->
                                viewHalfCard (Just (c.onInput <| Dict.remove (Uuid.toString uuid) uuids)) s.state.types s.state.configs s.state.identifiers s.state.grouped (Type.TType TType.Group) uuid
                            )
                   )
        , h2 <| "Select the groups this entity should belong to"
        , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
            (s.state.groups
                |> Dict.filter (\_ g -> containsScope s.state.types (c.mpuuid |> Maybe.map (\puuid -> HasUserType c.type_ puuid) |> Maybe.withDefault (HasType c.type_)) g.scope)
                |> Dict.map (\_ g -> g.uuid)
                |> Dict.values
                |> List.map
                    (\uuid ->
                        tClickableCard (c.onInput <| Dict.insert (Uuid.toString uuid) uuid uuids) s.state.types s.state.configs s.state.identifiers s.state.grouped (Type.TType TType.Group) uuid
                    )
                |> withDefaultContent (p "(There are no Groups yet)")
            )
        ]
