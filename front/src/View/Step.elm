module View.Step exposing (..)

import Element exposing (Attribute, Element)
import View exposing (button)


type Step step
    = Step step


type alias Model model step =
    { model | step : Step step, steps : List (Step step) }


nextOrValidate : Model model step -> msg -> msg -> Result String field -> Element msg
nextOrValidate m next validate result =
    case result of
        Ok _ ->
            if isLast m.step m.steps then
                button.primary validate "Validate and finish"

            else
                button.primary next "Next →"

        Err err ->
            button.disabled err "Next →"


onEnter : msg -> msg -> (String -> msg) -> Model model step -> Result String field -> Attribute msg
onEnter next validate warning m result =
    View.onEnter <|
        case result of
            Ok _ ->
                if isLast m.step m.steps then
                    validate

                else
                    next

            Err err ->
                warning err


indexOf : a -> List a -> Maybe Int
indexOf x =
    -- 1st index is 1
    List.indexedMap Tuple.pair >> List.filter (\z -> x == Tuple.second z) >> List.head >> Maybe.map Tuple.first


getItem : Int -> List a -> Maybe a
getItem i =
    List.indexedMap Tuple.pair >> List.filter (\x -> i == Tuple.first x) >> List.head >> Maybe.map Tuple.second


previousStep : a -> List a -> Maybe a
previousStep x xs =
    indexOf x xs |> Maybe.andThen (\i -> getItem (i - 1) xs)


nextStep : a -> List a -> Maybe a
nextStep x xs =
    indexOf x xs |> Maybe.andThen (\i -> getItem (i + 1) xs)


isLast : a -> List a -> Bool
isLast x xs =
    indexOf x xs |> Maybe.map ((==) (List.length xs - 1)) |> Maybe.withDefault False


isFirst : a -> List a -> Bool
isFirst x xs =
    indexOf x xs |> Maybe.map ((==) 0) |> Maybe.withDefault False
