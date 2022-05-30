module Page.Groups exposing (match, page, view)

import DictSet as Set
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Event
import REA.Entity as Entity exposing (Entity(..), toString)
import REA.Group exposing (Group)
import Route exposing (Route)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.Navbar as Navbar
import View.Radio as Radio


type alias Model =
    { route : Route
    , form : Form
    }


type Msg
    = Removed Group
    | Added Group
    | GotInput Form
    | Warning String


type alias Flags =
    { route : Route }


type alias Form =
    { name : String
    , entity : Maybe Entity
    , warning : String
    }


empty : Form
empty =
    { name = "", entity = Nothing, warning = "" }


validate : Form -> Maybe Group
validate f =
    if f.name == "" then
        Nothing

    else
        f.entity
            |> Maybe.andThen (\e -> Just { name = f.name, entity = e })


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
    case route of
        Route.Groups ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route, form = empty }, closeMenu s )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        GotInput form ->
            ( { model | form = form }, Effect.none )

        Added group ->
            ( { model
                | form = empty
              }
            , Shared.dispatch s <| Event.GroupAdded { name = group.name, entity = group.entity }
            )

        Removed group ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | warning = "" } }
            , Shared.dispatch s <| Event.GroupRemoved group.name
            )

        Warning w ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | warning = "" } }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Groups"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


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
                [ h1 "Groups"
                , if Set.size s.state.groups > 0 then
                    p "Existing groups:"

                  else
                    p "There are no groups yet. Create your first one!"
                , wrappedRow
                    [ spacing 10 ]
                    (s.state.groups
                        |> Set.toList
                        |> List.map (\g -> viewSmallCard (Removed g) g.name ("(Group of " ++ Entity.toPluralString g.entity ++ ")"))
                    )
                , column
                    [ spacing 20 ]
                    [ text "Add a new Group:"
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
                                Just <| Input.placeholder [] <| text "Name of the new group"
                            , label = Input.labelLeft [] <| text "Name"
                            }
                        ]
                    , row [ Font.size size.text.main ]
                        [ Radio.view
                            { title = "This will be a Group of:"
                            , options =
                                [ ( Process, "Processes" )
                                , ( Resource, "Resources" )
                                , ( Event, "Events" )
                                , ( Agent, "Agents" )
                                , ( Commitment, "Commitments" )
                                , ( Contract, "Contracts" )
                                ]
                            , selected = model.form.entity
                            , msg =
                                \e -> GotInput { form | entity = Just e }
                            }
                        ]
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
