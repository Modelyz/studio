module Zone.ListPage exposing (Flags, Model, Msg, match, page)

import Configuration exposing (Configuration)
import Configuration.View
import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Ident.Identifiable exposing (withIdentifiers)
import Message
import Route exposing (Route, redirect)
import Scope.View
import Shared
import Spa.Page
import State exposing (allHfromScope, allTfromScope)
import View exposing (..)
import View.Smallcard exposing (clickableRemovableCard, viewSmallCard)


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
        , view = view s
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
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.Configuration Route.Add )

        View zid ->
            ( model, Route.redirect s.navkey (Route.Entity Route.Configuration (Route.View zid)) |> Effect.fromCmd )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Configurations"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    flatContainer s
        Nothing
        "Configurations"
        [ button.primary Add "Add..."
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
                            (text <| Configuration.View.view s c)
                            (text <| Configuration.View.description s c)
                    )
                |> withDefaultContent (p "There are no Configurations yet. Create your first one!")
            )
        ]
