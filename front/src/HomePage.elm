module HomePage exposing (Flags, Model, Msg, match, page)

import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View, closeMenu)
import View.Style exposing (..)


type alias Model =
    { route : Route }


type alias Msg =
    ()


type alias Flags =
    { route : Route }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init s
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


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route }
    , closeMenu f s.menu
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
            [ cell "Process Types" "process-type/list"
            , cell "Resource Types" "resource-type/list"
            , cell "Event Types" "event-type/list"
            , cell "Agent Types" "agent-type/list"
            , cell "Commitment Types" "commitment-type/list"
            , cell "Contract Types" "contract-type/list"
            , cell "Group Types" "group-type/list"
            , cell "Processes" "process/list"
            , cell "Resources" "resource/list"
            , cell "Events" "event/list"
            , cell "Agents" "agent/list"
            , cell "Commitments" "commitment/list"
            , cell "Contracts" "contract/list"
            , cell "Groups" "group/list"
            , cell "IdentifierTypes" "identifier-type/list"
            , cell "ValueTypes" "value-type/list"
            , cell "Configuration" "config/list"
            ]
        ]
