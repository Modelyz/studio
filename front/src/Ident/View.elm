module Ident.View exposing (..)

-- TODO file seems unused

import Configuration exposing (Configuration(..))
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Hierarchy.Hierarchic as Hierarchic exposing (Hierarchic)
import Hierarchy.Type as HType
import Ident.Fragment as IdentFragment
import Ident.Identifiable as Identifiable exposing (Identifiable)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.Scope as Scope exposing (Scope(..))
import Item.Item as Item exposing (Item)
import Prng.Uuid as Uuid exposing (Uuid)
import Shared
import Typed.Type as TType
import Typed.Typed as Typed exposing (Typed)
import View exposing (..)
import View.Lang as Lang exposing (Lang(..))
import View.Style exposing (..)
import View.Type as ViewType
import Zone.Fragment as ZoneFragment
import Zone.Zone as Zone exposing (Zone(..))


type alias Model a =
    { a | identifiers : Dict String String }


type alias Config msg =
    { onEnter : msg
    , onInput : Identifier -> msg
    }


displayIdentifiers : String -> List Identifier -> Element msg
displayIdentifiers default is =
    -- Display the identifiers, using a default text if none
    row []
        (is
            |> List.map
                (\i ->
                    row [] <|
                        List.map (\f -> displayFragment f is) i.fragments
                )
            |> withDefaultContent (text default)
        )


displayFragment : IdentFragment.Fragment -> List Identifier -> Element msg
displayFragment fragment identifiers =
    case fragment of
        IdentFragment.Free value ->
            row [ height (px 30) ] [ text value ]

        IdentFragment.Fixed value ->
            row [ height (px 30) ] [ text value ]

        IdentFragment.Sequence padding step start value ->
            row [ height (px 30) ] [ text <| Maybe.withDefault "(Not yet assigned)" value ]

        _ ->
            text "(not implemented yet)"


displayScope : Scope -> Element msg
displayScope scope =
    -- TODO refactor to avoid Element here
    case scope of
        TScope t mtuid ->
            mtuid
                |> Maybe.map
                    (\uuid ->
                        row []
                            [ text <| TType.toString t ++ " of type "
                            , text <| Uuid.toString uuid

                            --                            , Identifiable.fromUuid all uuid
                            --                                |> Maybe.map (display allIdentifiers all allConfigs SmallcardTitle FR_fr t)
                            --                                |> Maybe.withDefault (text "(deleted type)")
                            ]
                    )
                |> Maybe.withDefault (text <| TType.toString t)

        HScope t mtuid ->
            mtuid
                |> Maybe.map
                    (\uuid ->
                        row []
                            [ text <| HType.toString t ++ " of type "
                            , text <| Uuid.toString uuid

                            --                            , Identifiable.fromUuid all uuid
                            --                                |> Maybe.map (display allIdentifiers all allConfigs SmallcardTitle FR_fr t)
                            --                                |> Maybe.withDefault (text "(deleted type)")
                            ]
                    )
                |> Maybe.withDefault (text <| HType.toString t)


type alias IdentColumn r msg =
    { header : Element msg, width : Length, view : r -> Element msg }


headerCell : String -> Element msg
headerCell =
    text >> el [ padding 5, Border.width 2, Border.color color.content.background, Background.color color.table.header.background ]


innerCell : String -> Element msg
innerCell =
    text >> el [ padding 5, Border.width 2, Border.color color.content.background, Background.color color.table.inner.background ]


displayIdentifierDict : String -> Dict String String -> Element msg
displayIdentifierDict default data =
    if Dict.size data > 0 then
        table [ width shrink, Background.color color.item.background ]
            { data = Dict.toList data
            , columns =
                [ { header = headerCell "Identifier", width = fill, view = Tuple.first >> innerCell }
                , { header = headerCell "Value", width = fill, view = Tuple.second >> innerCell }
                ]
            }

    else
        text "(none)"
