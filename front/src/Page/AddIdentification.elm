module Page.AddIdentification exposing (..)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import REA.Entity as Entity exposing (Entity, toPluralString)
import REA.Ident as Ident exposing (Fragment(..), Identification)
import Result exposing (andThen)
import Route exposing (Route)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.Navbar as Navbar
import View.Radio as Radio
import View.Step as Step exposing (isFirst, nextOrValidate, nextStep, previousStep)
import View.TagList exposing (..)


type Msg
    = InputName String
    | InputEntity Entity
    | InputUnique Bool
    | InputMandatory Bool
    | InputFormat (List Fragment)
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
    , taglist : List Fragment
    , warning : String
    , step : Step.Step Step
    , steps : List (Step.Step Step)
    }


type Step
    = Name
    | Entity
    | Options
    | Format


validate : Model -> Result String Identification
validate m =
    Result.map5
        Identification
        (checkEmptyString m.name "The name is Empty")
        (checkNothing m.entity "You must select an entity")
        (Ok m.unique)
        (Ok m.mandatory)
        (Ok m.taglist)


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
    -- TODO give the entity to create through the flags? /add/identification?step=2
    case route of
        Route.AddIdentification ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , previous = Route.Identifications
      , name = ""
      , entity = Nothing
      , unique = False
      , mandatory = False
      , taglist = []
      , warning = ""
      , steps = [ Step.Step Entity, Step.Step Options, Step.Step Format, Step.Step Name ]
      , step = Step.Step Entity
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
                        [ Shared.dispatch s <| Event.IdentificationAdded i
                        , goTo s Route.Identifications
                        ]
                    )

                Err err ->
                    ( { model | warning = err }, Effect.none )

        PreviousPage ->
            case previousStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, goTo s Route.Identifications )

        NextPage ->
            case nextStep model.step model.steps of
                Just x ->
                    ( { model | step = x }, Effect.none )

                Nothing ->
                    ( model, goTo s Route.Identifications )

        Cancel ->
            ( model, goTo s Route.Identifications )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding an Identification"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


buttonNext : Model -> Element Msg
buttonNext model =
    case model.step of
        Step.Step Entity ->
            nextOrValidate model NextPage Added (checkNothing model.entity "Please select an Entity")

        Step.Step Options ->
            nextOrValidate model NextPage Added (Ok model.name)

        Step.Step Format ->
            nextOrValidate model NextPage Added (checkEmptyList model.taglist "The format is still empty")

        Step.Step Name ->
            nextOrValidate model NextPage Added (checkEmptyString model.name "Please choose a name")


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        buttons : List (Element Msg)
        buttons =
            [ wrappedRow [ width fill, spacing 20 ]
                [ (if isFirst model.step model.steps then
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
                Step.Step Entity ->
                    row [ alignTop, width <| minimum 200 fill, Font.size size.text.h3 ]
                        [ Radio.view
                            { title = "Apply to Which Entity?"
                            , options = Entity.all |> List.map (\e -> ( e, toPluralString e ))
                            , selected = model.entity
                            , msg =
                                \e -> InputEntity e
                            }
                        ]

                Step.Step Options ->
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
                                , label = Input.labelRight [] <| text "This identification is mandatory"
                                }
                            ]
                        ]

                Step.Step Format ->
                    taglist model
                        { all = Ident.allFragments
                        , toString = Ident.fragmentToString
                        , toDesc = Ident.fragmentToDesc
                        , inputmsg = InputFormat
                        , label = "Format: "
                        , explain = h2 "Construct the format of your identification by clicking on the items below"
                        }

                Step.Step Name ->
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
                            , label = Input.labelAbove [ Font.size size.text.h3, paddingXY 0 10 ] <| text "Give a name to this new identification"
                            }
    in
    cardContent s
        "Adding an identification"
        buttons
        [ step
        ]
