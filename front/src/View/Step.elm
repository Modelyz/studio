module View.Step exposing (Model, Msg(..), Step(..), buttons, isLast, nextMsg, onEnter, update)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
import Route exposing (Route, redirectToView)
import Shared
import View exposing (..)
import View.Style exposing (color)


type Step step
    = Step step


type Msg
    = PreviousPage
    | NextPage
    | Cancel


type alias Model model step =
    { model
        | route : Route
        , step : Step step
        , steps : List (Step step)
        , warning : String
    }


buttonNext : Model model step -> Result String () -> Element Msg
buttonNext model result =
    case result of
        Ok _ ->
            if isLast model.step model.steps then
                none

            else
                button.primary NextPage "Next →"

        Err err ->
            button.disabled err "Next →"


onEnter : msg -> msg -> (String -> msg) -> Model model step -> Result String () -> Attribute msg
onEnter next validate warning model result =
    View.onEnter <|
        case result of
            Ok _ ->
                if isLast model.step model.steps then
                    validate

                else
                    next

            Err err ->
                warning err


nextMsg : Model model step -> (Msg -> msg) -> Msg -> msg -> msg
nextMsg model c next validate =
    if isLast model.step model.steps then
        validate

    else
        c next


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


buttons : Model m s -> Result String () -> List (Element Msg)
buttons model checkedStep =
    -- standard buttons for a floatingContainer with steps (Previous, Cancel, Next)
    [ (if isFirst model.step model.steps then
        button.disabled "This is the first page"

       else
        button.secondary PreviousPage
      )
        "← Previous"
    , button.secondary Cancel "Cancel"
    , buttonNext model checkedStep
    , if model.warning /= "" then
        paragraph [ Font.color color.text.warning ] [ text model.warning ]

      else
        none
    ]


update : Shared.Model -> Msg -> Model m s -> ( Model m s, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        PreviousPage ->
            case previousStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, redirectToView "list" s.navkey model.route |> Effect.fromCmd )

        NextPage ->
            case nextStep model.step model.steps of
                Just step ->
                    ( { model | step = step }, Effect.none )

                Nothing ->
                    ( model, redirectToView "list" s.navkey model.route |> Effect.fromCmd )

        Cancel ->
            ( model, redirectToView "list" s.navkey model.route |> Effect.fromCmd )
