module View.InputIdentifiers exposing (..)

import DictSet as Set exposing (DictSet)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import REA.Entity exposing (Entity)
import REA.Ident exposing (Fragment(..), Identifier, Name, fragmentToName, fragmentToString, fragmentToValue, identifierValue, updateIdentifier)
import Style exposing (size)
import View


type alias Model a =
    { a | identifiers : DictSet String Identifier }


type alias Config msg =
    { onEnter : msg
    , onInput : Identifier -> msg
    }


inputIdentifiers : Config msg -> Model a -> Element msg
inputIdentifiers c model =
    -- display an input field for each relevant identifier
    column []
        (model.identifiers
            |> Set.toList
            |> List.map
                (\identifier ->
                    row [] <|
                        List.indexedMap (\i f -> inputFragment c i f identifier) identifier.fragments
                )
        )


inputFragment : Config msg -> Int -> Fragment -> Identifier -> Element msg
inputFragment c index fragment ident =
    case fragment of
        Free value ->
            Input.text
                [ width <| minimum 200 fill
                , Input.focusedOnLoad
                , View.onEnter c.onEnter
                ]
                { onChange = \v -> c.onInput <| updateIdentifier index (Free v) ident
                , text = value
                , placeholder =
                    Just <| Input.placeholder [] <| text ident.name
                , label = Input.labelAbove [ Font.size size.text.h3, paddingXY 0 10 ] <| text ident.name
                }

        Sequence padding step value ->
            Input.text
                [ width <| minimum 200 fill
                , Input.focusedOnLoad
                , View.onEnter c.onEnter
                ]
                { onChange = \v -> c.onInput <| updateIdentifier index (Sequence padding step value) ident
                , text = ""
                , placeholder =
                    Just <| Input.placeholder [] <| text "(Automatically generated)"
                , label = Input.labelAbove [ Font.size size.text.h3, paddingXY 0 10 ] <| text ident.name
                }

        _ ->
            text "other"
