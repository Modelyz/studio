module View.InputIdentifiers exposing (..)

import DictSet as Set exposing (DictSet)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import REA.Entity exposing (Entity)
import REA.Ident exposing (Fragment(..), Identification, Identifier, Name, fragmentToName, fragmentToString, fragmentToValue, identifierValue, selectFrom, setName, updateIdentifier)
import Style exposing (size)
import View


type alias Model a =
    { a | identifiers : DictSet String Identifier }


type alias State b =
    { b | identifications : DictSet String Identification }


type alias Config msg =
    { onEnter : msg
    , onInput : Identifier -> msg
    }


inputIdentifierTypes : Config msg -> Entity -> Model a -> Element msg
inputIdentifierTypes c entity model =
    column []
        (model.identifiers
            |> Set.toList
            |> List.map (inputIdentifierType c model)
        )


inputIdentifierType : Config msg -> Model a -> Identifier -> Element msg
inputIdentifierType c m i =
    row [] <|
        List.indexedMap (\index fragment -> inputFragmentConf c index fragment i) i.fragments


inputFragmentConf : Config msg -> Int -> Fragment -> Identifier -> Element msg
inputFragmentConf c index fragment ident =
    case fragment of
        Free value ->
            row [] [ text "Free text" ]

        Fixed value ->
            Input.text
                [ width <| minimum 200 fill
                , Input.focusedOnLoad
                , View.onEnter c.onEnter
                ]
                { onChange = \n -> c.onInput <| updateIdentifier index (Free value) ident
                , text = ""
                , placeholder =
                    Just <| Input.placeholder [] <| text ident.name
                , label = Input.labelAbove [ Font.size size.text.h3, paddingXY 0 10 ] <| text ident.name
                }

        Sequence padding step value ->
            el [] <| text <| String.fromInt value

        _ ->
            text "other"


inputIdentifiers : Config msg -> Entity -> Model a -> Element msg
inputIdentifiers c entity model =
    column []
        (model.identifiers
            |> Set.toList
            |> List.map (inputIdentifier c model)
        )


inputIdentifier : Config msg -> Model a -> Identifier -> Element msg
inputIdentifier c m i =
    row [] <|
        List.indexedMap (\index fragment -> inputFragment c index fragment i) i.fragments


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

        _ ->
            text "other"
