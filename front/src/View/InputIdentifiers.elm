module View.InputIdentifiers exposing (..)

import DictSet as Set exposing (DictSet)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import REA.Entity exposing (Entity)
import REA.Ident exposing (Fragment(..), Identifier, Name, fragmentToValue, identifierValue, selectFrom, updateIdentifier)
import Style exposing (size)
import View


type alias Model a =
    { a | identifiers : DictSet String Identifier }


type alias Config msg =
    { onEnter : msg
    , onInput : Identifier -> msg
    }


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
inputFragment c index f i =
    case f of
        Free name value ->
            Input.text
                [ width <| minimum 200 fill
                , Input.focusedOnLoad
                , View.onEnter c.onEnter
                ]
                { onChange = \v -> c.onInput <| updateIdentifier index (Free name v) i
                , text = value
                , placeholder =
                    Just <| Input.placeholder [] <| text i.name
                , label = Input.labelAbove [ Font.size size.text.h3, paddingXY 0 10 ] <| text i.name
                }

        _ ->
            text "other"
