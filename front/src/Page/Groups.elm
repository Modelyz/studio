module Page.Groups exposing (match, page, view)

import DictSet as Set
import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import REA.Entity exposing (Entity(..))
import REA.Group exposing (Group)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View, color)
import View.Radio as Radio


type alias Form =
    { name : String
    , entity : Maybe Entity
    }


empty : Form
empty =
    { name = "", entity = Nothing }


validate : Form -> Maybe Group
validate f =
    if f.name == "" then
        Nothing

    else
        Just f.name
            |> Maybe.andThen
                (\n ->
                    f.entity
                        |> Maybe.andThen (\e -> Just { name = n, entity = e })
                )


type alias Model =
    { form : Form
    , warning : String
    }


type Msg
    = Removed Group
    | Added Group
    | GotInput Form
    | Warning String


type alias Flags =
    ()


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
        Route.Groups ->
            Just ()

        _ ->
            Nothing


init : Flags -> ( Model, Effect Shared.Msg Msg )
init _ =
    ( { form = empty, warning = "" }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Groups"
    , attributes = []
    , element = viewContent s model
    }


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        GotInput form ->
            ( { model | form = form }, Effect.none )

        Removed group ->
            ( { model | warning = "" }
            , Shared.dispatch s <| Event.GroupRemoved { name = group.name, entity = group.entity }
            )

        Added group ->
            ( { model
                | form = empty
                , warning = ""
              }
            , Shared.dispatch s <| Event.GroupAdded { name = group.name, entity = group.entity }
            )

        Warning w ->
            ( { model | warning = w }, Effect.none )


viewThumbnail : Group -> Element Msg
viewThumbnail g =
    column
        [ htmlAttribute <| Attr.id g.name ]
        [ row []
            [ text g.name
            , Input.button []
                { onPress = Just <| Removed g, label = text "X" }
            ]
        ]


viewContent : Shared.Model -> Model -> Element Msg
viewContent s model =
    let
        form =
            model.form
    in
    column
        []
        [ paragraph [ Font.color color.text.warning ] [ text model.warning ]
        , column
            []
            [ paragraph []
                [ text "Groups"
                ]
            , paragraph [] [ text "What kind of groups may be created" ]
            ]
        , column
            []
            [ column
                []
                ((if Set.size s.state.processTypes > 0 then
                    text "Current groups:"

                  else
                    column [] []
                 )
                    :: (s.state.groups
                            |> Set.toList
                            |> List.map viewThumbnail
                       )
                )
            , column
                []
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
                        , label = Input.labelLeft [] <| text "Group"
                        }
                    ]
                , row []
                    [ Radio.view
                        { title = "This is a Group of:"
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
            , column []
                [ column
                    []
                    [ Input.button []
                        { onPress =
                            Maybe.map Added (validate model.form)
                                |> Maybe.withDefault (Warning "Incomplete form")
                                |> Just
                        , label = text "Add"
                        }
                    ]
                ]
            ]
        ]
