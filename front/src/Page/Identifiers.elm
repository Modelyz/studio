module Page.Identifiers exposing (match, page, view)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import REA.Entity as Entity exposing (Entity, toPluralString)
import REA.Identifier as I exposing (..)
import REA.Identifier.Portion as Portion exposing (Portion(..))
import Result exposing (andThen)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (..)
import View.Radio as Radio


type alias Model =
    { form : Form
    }


type Msg
    = Removed String
    | Added Identifier
    | GotInput Form
    | Warning String


type alias Flags =
    ()


type alias Form =
    { name : String
    , entity : Maybe Entity
    , unique : Bool
    , mandatory : Bool
    , format : List Portion
    , warning : String
    }


empty : Form
empty =
    { name = ""
    , entity = Nothing
    , unique = False
    , mandatory = False
    , format = []
    , warning = ""
    }


fromForm : Form -> Result String Identifier
fromForm f =
    case f.entity of
        Just e ->
            Ok (Identifier f.name e f.unique f.mandatory f.format)

        Nothing ->
            Err "No entity is selected"


check : String -> (Form -> Bool) -> Form -> Result String Form
check err c f =
    if c f then
        Ok f

    else
        Err err


validate : Form -> Result String Identifier
validate =
    check "The name is empty" (\f -> String.length f.name > 0)
        >> Result.andThen (check "The format is not defined" (\f -> List.length f.format > 0))
        >> Result.andThen fromForm


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init
        , update = update s
        , view = view s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.Identifiers ->
            Just ()

        _ ->
            Nothing


init : Flags -> ( Model, Effect Shared.Msg Msg )
init _ =
    ( { form = empty }, Effect.none )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        GotInput form ->
            ( { model | form = form }, Effect.none )

        Added i ->
            ( { model
                | form = empty
              }
            , Shared.dispatch s <| Event.IdentifierAdded i
            )

        Removed i ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | warning = "" } }
            , Shared.dispatch s <| Event.IdentifierRemoved i
            )

        Warning w ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | warning = w } }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Event Types"
    , attributes = []
    , element = viewContent s model
    }


viewContent : Shared.Model -> Model -> Element Msg
viewContent s model =
    let
        form =
            model.form
    in
    column [ width fill, alignTop, padding 20 ]
        [ column [ Border.shadow shadowStyle, padding 20, centerX, alignTop ]
            [ column
                [ spacing 20 ]
                [ h1 "Identifiers"
                , if Set.size s.state.eventTypes > 0 then
                    p "Existing Identifiers:"

                  else
                    p "There are no Identifiers yet. Create your first one!"
                , wrappedRow
                    [ spacing 10 ]
                    (s.state.identifiers
                        |> Set.toList
                        |> List.sortBy .name
                        |> List.map (\i -> viewSmallCard (Removed i.name) i.name ("for " ++ Entity.toPluralString i.entity))
                    )
                , row [ width fill, Border.width 1, Border.color color.content.separator ] []
                , column [ width fill ]
                    [ h2 "Add a new Identifier:"
                    , wrappedRow [ padding 20, width fill, spacing 50 ]
                        [ el [ alignTop ] <|
                            Input.text
                                [ width <| minimum 200 fill
                                , Input.focusedOnLoad
                                , View.onEnter <|
                                    case validate model.form of
                                        Ok f ->
                                            Added f

                                        Err err ->
                                            Warning ("Error: " ++ err)
                                ]
                                { onChange = \n -> GotInput { form | name = n }
                                , text = model.form.name
                                , placeholder =
                                    Just <| Input.placeholder [] <| text "Name of the new Identifier"
                                , label = Input.labelAbove [ Font.size size.text.h3, paddingXY 0 10 ] <| text "Name"
                                }
                        , row [ alignTop, width <| minimum 200 fill, Font.size size.text.h3 ]
                            [ Radio.view
                                { title = "Apply to Which Entity?"
                                , options = Entity.all |> List.map (\e -> ( e, toPluralString e ))
                                , selected = model.form.entity
                                , msg =
                                    \e -> GotInput { form | entity = Just e }
                                }
                            ]
                        , column [ alignTop, width <| minimum 200 fill, spacing 10 ]
                            [ h3 "Options:"
                            , row [ Font.size size.text.main ]
                                [ Input.checkbox
                                    []
                                    { onChange = \u -> GotInput { form | unique = u }
                                    , icon = Input.defaultCheckbox
                                    , checked = form.unique
                                    , label = Input.labelRight [] <| text "Each value is unique"
                                    }
                                ]
                            , row [ Font.size size.text.main ]
                                [ Input.checkbox
                                    []
                                    { onChange = \u -> GotInput { form | mandatory = u }
                                    , icon = Input.defaultCheckbox
                                    , checked = form.mandatory
                                    , label = Input.labelRight [] <| text "This identifier is mandatory"
                                    }
                                ]
                            ]
                        , column [ alignTop, spacing 10, width <| minimum 200 fill ]
                            [ h3 "Format:"
                            , wrappedRow [ Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
                                List.append
                                    (List.indexedMap
                                        (\i p ->
                                            row [ Background.color color.item.selected ]
                                                [ el [ padding 5 ] (text <| Portion.toString p)
                                                , button.secondary
                                                    { onPress =
                                                        Just <|
                                                            GotInput
                                                                { form
                                                                    | format =
                                                                        form.format
                                                                            |> List.indexedMap Tuple.pair
                                                                            |> List.filter (\( j, q ) -> j /= i)
                                                                            |> List.map Tuple.second
                                                                }
                                                    , label = text "×"
                                                    }
                                                ]
                                        )
                                        form.format
                                    )
                                    [ button.primary { onPress = Just <| GotInput { form | format = form.format ++ [ Free "plop" ] }, label = text "+" } ]
                            ]
                        ]
                    , row [ paddingXY 0 20 ]
                        [ button.primary
                            { onPress =
                                Just <|
                                    case validate model.form of
                                        Ok f ->
                                            Added f

                                        Err err ->
                                            Warning ("Error: " ++ err)
                            , label = text "Set"
                            }
                        , if model.form.warning /= "" then
                            paragraph [ Font.color color.text.warning ] [ text model.form.warning ]

                          else
                            none
                        ]
                    ]
                ]
            ]
        ]
