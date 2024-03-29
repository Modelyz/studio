module View.Step exposing (Model, Msg(..), Step(..), buttons, nextMsg, onEnter, update)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
import Route exposing (Route)
import Shared
import Util exposing (indexOf)
import View exposing (..)
import View.Style exposing (color)


type Step step
    = Step step


type Msg
    = PreviousPage
    | NextPage
    | Added
    | Cancel


type alias Model model step =
    { model
        | route : Route
        , step : Step step
        , steps : List (Step step)
        , warning : String
    }


buttonNextOrValidate : msg -> Model model step -> Result String field -> Element Msg
buttonNextOrValidate _ model result =
    -- TODO try to suppress using at View.Step.nextMsg
    case result of
        Ok _ ->
            if isLast model.step model.steps then
                button.primary (Ok Added) "Validate and finish"

            else
                button.primary (Ok NextPage) "Next →"

        Err err ->
            button.disabled (Err err) "Next →"


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


nextMsg : Model model step -> (Msg -> msg) -> Msg -> Msg -> msg
nextMsg model c next added =
    if isLast model.step model.steps then
        c added

    else
        c next


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
        button.disabled (Err "This is the first page")

       else
        button.secondary (Ok PreviousPage)
      )
        "← Previous"
    , button.secondary (Ok Cancel) "Cancel"
    , buttonNextOrValidate (Ok Added) model checkedStep
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
                Just step ->
                    ( { model | step = step }, Effect.none )

                Nothing ->
                    ( model, Route.goBack s.navkey model.route |> Effect.fromCmd )

        NextPage ->
            case nextStep model.step model.steps of
                Just step ->
                    ( { model | step = step }, Effect.none )

                Nothing ->
                    ( model, Route.goBack s.navkey model.route |> Effect.fromCmd )

        Added ->
            ( model, Effect.none )

        Cancel ->
            ( model
            , Route.goBack s.navkey model.route
                |> Effect.fromCmd
            )
