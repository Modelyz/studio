module Ident.View exposing (..)

-- TODO file seems unused

import Configuration exposing (Configuration(..))
import Dict exposing (Dict)
import Dict exposing (Dict)
import Element exposing (..)
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
import View exposing (withDefaultContent)
import View.Lang as Lang exposing (Lang(..))
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
displayIdentifiers default identifiers =
    row []
        (identifiers
            |> List.map
                (\identifier ->
                    row [] <|
                        List.map (\f -> displayFragment f identifiers) identifier.fragments
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
                            --                                |> Maybe.map (display allIdentifiers all allConfigs SmallcardItemTitle FR_fr t)
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
                            --                                |> Maybe.map (display allIdentifiers all allConfigs SmallcardItemTitle FR_fr t)
                            --                                |> Maybe.withDefault (text "(deleted type)")
                            ]
                    )
                |> Maybe.withDefault (text <| HType.toString t)
