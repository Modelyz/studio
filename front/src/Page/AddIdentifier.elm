module Page.AddIdentifier exposing (..)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import Page exposing (..)
import REA.Entity as Entity exposing (Entity, toPluralString)
import REA.Identifier as I exposing (..)
import REA.Identifier.Portion as Portion exposing (Portion(..))
import Result exposing (andThen)
import Route exposing (Route)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.Navbar as Navbar
import View.Radio as Radio
import View.TagList exposing (..)


type Msg
    = InputName String
    | InputEntity Entity
    | InputUnique Bool
    | InputMandatory Bool
    | InputFormat (List Portion)
    | Warning String
    | PreviousPage
    | NextPage
    | Cancel
    | Added


type alias Flags =
    { route : Route }


type alias Model =
    { route : Route
    , previous : Route
    , name : String
    , entity : Maybe Entity
    , unique : Bool
    , mandatory : Bool
    , taglist : List Portion
    , warning : String
    , step : Step
    }


type Step
    = Name
    | Entity
    | Options
    | Format


steps : List Step
steps =
    [ Entity, Options, Format, Name ]


validate : Model -> Result String Identifier
validate m =
    Result.map5
        Identifier
        (checkEmptyString m.name "The name is Empty")
        (checkNothing m.entity "You must select an entity")
        (Ok m.unique)
        (Ok m.mandatory)
        (Ok m.taglist)


indexOf : a -> List a -> Maybe Int
indexOf x =
    -- 1st index is 1
    List.indexedMap Tuple.pair >> List.filter (\z -> x == Tuple.second z) >> List.head >> Maybe.map Tuple.first


getItem : Int -> List a -> Maybe a
getItem i =
    List.indexedMap Tuple.pair >> List.filter (\x -> i == Tuple.first x) >> List.head >> Maybe.map Tuple.second


previous : a -> List a -> Maybe a
previous x xs =
    indexOf x xs |> Maybe.andThen (\i -> getItem (i - 1) xs)


next : a -> List a -> Maybe a
next x xs =
    indexOf x xs |> Maybe.andThen (\i -> getItem (i + 1) xs)


isLast : a -> List a -> Bool
isLast x xs =
    indexOf x xs |> Maybe.map ((==) (List.length xs - 1)) |> Maybe.withDefault False


isFirst : a -> List a -> Bool
isFirst x xs =
    indexOf x xs |> Maybe.map ((==) 0) |> Maybe.withDefault False


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init s
        , update = update s
        , view = view s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    -- TODO give the entity to create through the flags? /add/identifier?step=2
    case route of
        Route.AddIdentifier ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , previous = Route.Identifiers
      , name = ""
      , entity = Nothing
      , unique = False
      , mandatory = False
      , taglist = []
      , warning = ""
      , step = Entity
      }
    , closeMenu s
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        InputName x ->
            ( { model | name = x }, Effect.none )

        InputEntity x ->
            ( { model | entity = Just x }, Effect.none )

        InputUnique x ->
            ( { model | unique = x }, Effect.none )

        InputMandatory x ->
            ( { model | mandatory = x }, Effect.none )

        InputFormat x ->
            ( { model | taglist = x }, Effect.none )

        Warning err ->
            ( { model | warning = err }, Effect.none )

        Added ->
            case validate model of
                Ok i ->
                    ( model
                    , Effect.batch
                        [ Shared.dispatch s <| Event.IdentifierAdded i
                        , goTo s Route.Identifiers
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        PreviousPage ->
            case previous model.step steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, goTo s Route.Identifiers )

        NextPage ->
            case next model.step steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, goTo s Route.Identifiers )

        Cancel ->
            ( model, goTo s Route.Identifiers )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Event Types"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


buttonNext : Model -> Element Msg
buttonNext model =
    case model.step of
        Entity ->
            case checkNothing model.entity "Please select an Entity" of
                Ok _ ->
                    button.primary NextPage "Next →"

                Err err ->
                    button.disabled err "Next →"

        Options ->
            button.primary NextPage "Next →"

        Format ->
            case checkEmptyList model.taglist "The format is still empty" of
                Ok _ ->
                    button.primary NextPage "Next →"

                Err err ->
                    button.disabled err "Next →"

        Name ->
            case checkEmptyString model.name "Please choose a name" of
                Ok _ ->
                    button.primary Added "Validate and add the identifier"

                Err err ->
                    button.disabled err "Validate and add the identifier"


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        buttons : List (Element Msg)
        buttons =
            [ wrappedRow [ width fill, spacing 20 ]
                [ (if isFirst model.step steps then
                    button.disabled "This is the first page"

                   else
                    button.secondary PreviousPage
                  )
                    "← Previous"
                , button.secondary Cancel "Cancel"
                , buttonNext model
                , if model.warning /= "" then
                    paragraph [ Font.color color.text.warning ] [ text model.warning ]

                  else
                    none
                ]
            ]

        step =
            case model.step of
                Entity ->
                    row [ alignTop, width <| minimum 200 fill, Font.size size.text.h3 ]
                        [ Radio.view
                            { title = "Apply to Which Entity?"
                            , options = Entity.all |> List.map (\e -> ( e, toPluralString e ))
                            , selected = model.entity
                            , msg =
                                \e -> InputEntity e
                            }
                        ]

                Options ->
                    column [ alignTop, width <| minimum 200 fill, spacing 10 ]
                        [ h3 "Options:"
                        , row [ Font.size size.text.main ]
                            [ Input.checkbox
                                []
                                { onChange = InputUnique
                                , icon = Input.defaultCheckbox
                                , checked = model.unique
                                , label = Input.labelRight [] <| text "Each value is unique"
                                }
                            ]
                        , row [ Font.size size.text.main ]
                            [ Input.checkbox
                                []
                                { onChange = InputMandatory
                                , icon = Input.defaultCheckbox
                                , checked = model.mandatory
                                , label = Input.labelRight [] <| text "This identifier is mandatory"
                                }
                            ]
                        ]

                Format ->
                    taglist model
                        { all = Portion.all
                        , toString = Portion.toString
                        , toDesc = Portion.toDesc
                        , inputmsg = InputFormat
                        , label = "Format: "
                        , explain = h2 "Construct the format of your identifier by clicking on the items below"
                        }

                Name ->
                    el [ alignTop ] <|
                        Input.text
                            [ width <| minimum 200 fill
                            , Input.focusedOnLoad
                            , View.onEnter Added
                            ]
                            { onChange = InputName
                            , text = model.name
                            , placeholder =
                                Just <| Input.placeholder [] <| text "Name"
                            , label = Input.labelAbove [ Font.size size.text.h3, paddingXY 0 10 ] <| text "Give a name to this new identifier"
                            }
    in
    cardContent s
        "Adding an identifier"
        buttons
        [ step
        ]
