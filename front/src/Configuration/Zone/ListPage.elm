module Configuration.Zone.ListPage exposing (Flags, Model, Msg, match, page)

import Configuration exposing (Configuration)
import Configuration.View
import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Message
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import View exposing (..)
import View.Smallcard exposing (clickableRemovableCard)


type alias Model =
    { route : Route }


type Msg
    = Removed Configuration
    | View String
    | Add


type alias Flags =
    { route : Route }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init s
        , update = update s
        , view = view
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.Entity Route.Configuration (Route.List _) ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route }, closeMenu f s.menu )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed c ->
            ( model
            , Shared.dispatch s <| Message.Unconfigured c
            )

        Add ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.Configuration (Route.Add Nothing) )

        View zid ->
            ( model, Route.redirect s.navkey (Route.Entity Route.Configuration (Route.View zid Nothing)) |> Effect.fromCmd )


view : Model -> View Msg
view model =
    { title = "Configurations"
    , attributes = []
    , element = viewContent
    , route = model.route
    }


viewContent : Shared.Model -> Element Msg
viewContent s =
    flatContainer s
        Nothing
        "Configurations"
        [ button.primary (Ok Add) "Add..."
        ]
        none
        none
        [ wrappedRow
            [ spacing 10 ]
            (s.state.configs
                |> Dict.values
                |> List.map
                    (\c ->
                        clickableRemovableCard (View <| Configuration.compare c)
                            (Removed c)
                            (text <| Configuration.View.description s c)
                            (text <| Configuration.View.view c)
                    )
                |> withDefaultContent (p "There are no Configurations yet. Create your first one!")
            )
        ]
