module HomePage exposing (Flags, Model, Msg, match, page)

import Dict
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as Background
import Prng.Uuid as Uuid
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View, closeMenu, h1)
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
    , element = \_ -> viewContent s
    , route = model.route
    }


cell : Shared.Model -> Route -> Element Msg
cell s route =
    column [ width (px 200), height (px 200) ]
        [ E.link [ mouseOver itemHoverstyle, centerX, centerY, width fill, height fill, Background.color color.item.background ] { url = Route.toString route, label = column [ alignTop, centerX, width fill, padding 10, Background.color (Route.toColor route) ] [ text <| Route.toDesc s.state route ] } ]


viewContent : Shared.Model -> Element Msg
viewContent s =
    if s.admin then
        adminHome s

    else
        userHome s


adminHome : Shared.Model -> Element Msg
adminHome s =
    column [ width fill, alignTop, padding 20 ]
        [ h1 "Entity Types"
        , wrappedRow [ height fill, width fill, spacing 20, padding 20 ]
            (List.map (\es -> cell s (Route.Entity es (Route.List Nothing))) Route.allTypes)
        , h1 "Behaviours and Configuration"
        , wrappedRow [ height fill, width fill, spacing 20, padding 20 ]
            (List.map (\es -> cell s (Route.Entity es (Route.List Nothing))) Route.allBehaviours)
        ]


userHome : Shared.Model -> Element Msg
userHome s =
    column [ width fill, alignTop, padding 20 ]
        [ wrappedRow [ height fill, width fill, spacing 20, padding 20 ]
            ((s.state.resourceTypes |> Dict.values |> List.map (\e -> cell s (Route.Entity Route.Resource (Route.List <| Just <| Uuid.toString e.uuid))))
                ++ (s.state.eventTypes |> Dict.values |> List.map (\e -> cell s (Route.Entity Route.Event (Route.List <| Just <| Uuid.toString e.uuid))))
                ++ (s.state.agentTypes |> Dict.values |> List.map (\e -> cell s (Route.Entity Route.Agent (Route.List <| Just <| Uuid.toString e.uuid))))
                ++ (s.state.commitmentTypes |> Dict.values |> List.map (\e -> cell s (Route.Entity Route.Commitment (Route.List <| Just <| Uuid.toString e.uuid))))
                ++ (s.state.contractTypes |> Dict.values |> List.map (\e -> cell s (Route.Entity Route.Contract (Route.List <| Just <| Uuid.toString e.uuid))))
                ++ (s.state.processTypes |> Dict.values |> List.map (\e -> cell s (Route.Entity Route.Process (Route.List <| Just <| Uuid.toString e.uuid))))
                ++ (s.state.groupTypes |> Dict.values |> List.map (\e -> cell s (Route.Entity Route.Group (Route.List <| Just <| Uuid.toString e.uuid))))
            )
        ]
