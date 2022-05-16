module Page.Home exposing (match, page, view)

import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Route exposing (Route)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (View)
import View.Navbar as Navbar


type alias Model =
    { route : Route }


type alias Msg =
    ()


type alias Flags =
    { route : Route }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init
        , update = update
        , view = view s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.Home ->
            Just { route = route }

        _ ->
            Nothing


init : Flags -> ( Model, Effect Shared.Msg Msg )
init f =
    ( { route = f.route }
    , Effect.none
    )


update : Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update _ model =
    ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Home"
    , attributes = []
    , element = \_ -> viewContent
    , route = model.route
    }


cell : String -> String -> Element Msg
cell title link =
    column [ width (px 250), height (px 250) ]
        [ E.link [ mouseOver itemHoverstyle, centerX, centerY, width fill, height fill, Background.color color.item.background ] { url = link, label = column [ alignTop, centerX, padding 10 ] [ text title ] } ]


viewContent : Element Msg
viewContent =
    column [ width fill, alignTop ]
        [ wrappedRow [ height fill, width fill, spacing 20, padding 20 ]
            [ cell "Process Types" "process-types"
            , cell "Resource Types" "resource-types"
            , cell "Event Types" "event-types"
            , cell "Agent Types" "agent-types"
            , cell "Commitment Types" "commitment-types"
            , cell "Contract Types" "contract-types"
            , cell "Groups" "groups"
            , cell "Identifiers" "identifiers"
            ]
        ]
