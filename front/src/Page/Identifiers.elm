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
import View.Navbar as Navbar
import View.Radio as Radio
import View.Wizard as Wizard


type alias Model =
    { route : Route
    , form : Form
    }


type Msg
    = Removed String
    | Added Identifier
    | GotInput Form
    | Warning String
    | Wizard
    | PreviousPage Int
    | NextPage Int
    | Cancel


type alias Flags =
    { route : Route
    }


type alias Form =
    { name : String
    , entity : Maybe Entity
    , unique : Bool
    , mandatory : Bool
    , format : List Portion
    , warning : String
    , step : Maybe Int
    }


empty : Form
empty =
    { name = ""
    , entity = Nothing
    , unique = False
    , mandatory = False
    , format = []
    , warning = ""
    , step = Nothing
    }


fromForm : Form -> Result String Identifier
fromForm f =
    case f.entity of
        Just e ->
            Ok (Identifier f.name e f.unique f.mandatory f.format)

        Nothing ->
            Err "No entity is selected"


checkField : String -> (Form -> Bool) -> Form -> Result String Form
checkField err c f =
    if c f then
        Ok f

    else
        Err err


validate : Form -> Result String Identifier
validate =
    checkField "The name is empty" (\f -> String.length f.name > 0)
        >> Result.andThen (checkField "The format is not defined" (\f -> List.length f.format > 0))
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
            Just { route = route }

        _ ->
            Nothing


init : Flags -> ( Model, Effect Shared.Msg Msg )
init f =
    ( { route = f.route, form = empty }, Effect.none )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    let
        form =
            model.form
    in
    case msg of
        GotInput f ->
            ( { model | form = f }, Effect.none )

        Added i ->
            ( { model
                | form = empty
              }
            , Shared.dispatch s <| Event.IdentifierAdded i
            )

        Removed i ->
            ( { model | form = { form | warning = "" } }
            , Shared.dispatch s <| Event.IdentifierRemoved i
            )

        Warning w ->
            ( { model | form = { form | warning = w } }, Effect.none )

        PreviousPage n ->
            ( { model
                | form =
                    { form
                        | step =
                            form.step
                                |> Maybe.map (\i -> i - 1)
                                |> Maybe.andThen
                                    (\i ->
                                        if List.member i (List.range 1 n) then
                                            Just i

                                        else
                                            Nothing
                                    )
                    }
              }
            , Effect.none
            )

        NextPage n ->
            ( { model
                | form =
                    { form
                        | step =
                            form.step
                                |> Maybe.map ((+) 1)
                                |> Maybe.andThen
                                    (\i ->
                                        if List.member i (List.range 1 n) then
                                            Just i

                                        else
                                            Nothing
                                    )
                    }
              }
            , Effect.none
            )

        Wizard ->
            ( { model | form = { form | step = Just 1 } }, Effect.none )

        Cancel ->
            ( { model | form = empty }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Event Types"
    , attributes = []
    , element =
        row
            [ width fill, height fill ]
            [ Navbar.view s model
            , viewContent s model
            ]
    }


viewContent : Shared.Model -> Model -> Element Msg
viewContent s model =
    let
        form =
            model.form

        check =
            case validate form of
                Ok f ->
                    Added f

                Err err ->
                    Warning ("Error: " ++ err)
    in
    Wizard.view
        model
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
        ]
        { title = "Add a new Identifier"
        , context = "Adding a new identifier"
        , steps =
            [ row [ alignTop, width <| minimum 200 fill, Font.size size.text.h3 ]
                [ Radio.view
                    { title = "Apply to Which Entity?"
                    , options = Entity.all |> List.map (\e -> ( e, toPluralString e ))
                    , selected = form.entity
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
                , wrappedRow [ width <| minimum 50 shrink, Border.width 2, padding 3, spacing 4, Border.color color.item.border ] <|
                    List.append
                        (if List.isEmpty form.format then
                            [ el [ padding 5, Font.color color.text.disabled ] (text "Empty") ]

                         else
                            []
                        )
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
                , p "Construct the format of your identifier by clicking on the items below"
                , wrappedRow [ Border.width 2, padding 10, spacing 10, Border.color color.item.border ] <|
                    List.map
                        (\p ->
                            column [ Background.color color.item.background, mouseOver itemHoverstyle, width (px 250), height (px 150) ]
                                [ row [ alignLeft ]
                                    [ button.primary { onPress = Just <| GotInput { form | format = form.format ++ [ p ] }, label = text "+" }
                                    , el [ paddingXY 10 0 ] (text <| Portion.toString p)
                                    ]
                                , paragraph [ padding 10, Font.size size.text.main ] [ text <| Portion.toDesc p ]
                                ]
                        )
                        Portion.all
                ]
            , el [ alignTop ] <|
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
                    , text = form.name
                    , placeholder =
                        Just <| Input.placeholder [] <| text "Name"
                    , label = Input.labelAbove [ Font.size size.text.h3, paddingXY 0 10 ] <| text "Give a name to this new identifier"
                    }
            ]
        , button =
            row [ paddingXY 0 20 ]
                [ button.primary
                    { onPress =
                        Just check
                    , label = text "Set"
                    }
                , if form.warning /= "" then
                    paragraph [ Font.color color.text.warning ] [ text form.warning ]

                  else
                    none
                ]
        , open = Wizard
        , next = NextPage
        , previous = PreviousPage
        , ok = check
        , cancel = Cancel
        }
