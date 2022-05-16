module Page.ProcessTypes exposing (match, page, view)

import DictSet as Set
import Effect exposing (Effect)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import REA.ProcessType exposing (ProcessType)
import Route exposing (Route)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.Navbar as Navbar


type alias Model =
    { route : Route
    , form : Form
    }


type Msg
    = Removed ProcessType
    | Added ProcessType
    | GotInput Form
    | Warning String


type alias Flags =
    { route : Route }


type alias Form =
    { name : String, warning : String }


empty : Form
empty =
    { name = "", warning = "" }


validate : Form -> Maybe ProcessType
validate f =
    if f.name == "" then
        Nothing

    else
        Just { name = f.name }


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
        Route.ProcessTypes ->
            Just { route = route }

        _ ->
            Nothing


init : Flags -> ( Model, Effect Shared.Msg Msg )
init f =
    ( { route = f.route, form = empty }, Effect.none )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        GotInput form ->
            ( { model | form = form }, Effect.none )

        Added ptype ->
            ( { model
                | form = empty
              }
            , Shared.dispatch s <| Event.ProcessTypeChanged { ptype = ptype }
            )

        Removed ptype ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | warning = "" } }
            , Shared.dispatch s <| Event.ProcessTypeRemoved { ptype = ptype.name }
            )

        Warning w ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | warning = w } }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Process Types"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewThumbnail : ProcessType -> Element Msg
viewThumbnail pt =
    row
        []
        [ row
            [ htmlAttribute <| Attr.id pt.name ]
            [ Input.button []
                { onPress = Just <| Removed pt, label = text pt.name }
            ]
        ]


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    let
        form =
            model.form
    in
    column [ width fill, alignTop, padding 20 ]
        [ column [ Border.shadow shadowStyle, padding 20, centerX, alignTop ]
            [ column
                [ spacing 20 ]
                [ h1 "Process Types"
                , if Set.size s.state.processTypes > 0 then
                    p "Existing Process Types:"

                  else
                    p "There are no Process Types yet. Create your first one!"
                , wrappedRow
                    [ spacing 10 ]
                    (s.state.processTypes
                        |> Set.toList
                        |> List.map (\pt -> viewSmallCard (Removed pt) pt.name "")
                    )
                , column
                    [ spacing 20 ]
                    [ text "Add a new Process Type:"
                    , row []
                        [ Input.text
                            [ Input.focusedOnLoad
                            , Maybe.map Added (validate model.form)
                                |> Maybe.withDefault (Warning "Incomplete form")
                                |> View.onEnter
                            ]
                            { onChange = \n -> GotInput { form | name = n }
                            , text = model.form.name
                            , placeholder =
                                Just <| Input.placeholder [] <| text "Name of the new Process Type"
                            , label = Input.labelLeft [] <| text "Name"
                            }
                        ]
                    , row [ spacing 20 ]
                        [ button.primary
                            (Maybe.map Added (validate model.form)
                                |> Maybe.withDefault (Warning "Incomplete form")
                            )
                            "Add"
                        , if model.form.warning /= "" then
                            paragraph [ Font.color color.text.warning ] [ text model.form.warning ]

                          else
                            none
                        ]
                    ]
                ]
            ]
        ]
