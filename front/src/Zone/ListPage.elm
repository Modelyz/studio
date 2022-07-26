module Zone.ListPage exposing (match, page, view)

import Configuration exposing (Configuration(..))
import Configuration.View
import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Ident.Scope as Scope
import Message
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import View exposing (..)
import View.Smallcard exposing (viewSmallCard)


type alias Model =
    { route : Route }


type Msg
    = Removed Configuration
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
        Route.ConfigurationList ->
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
            ( model, redirect s.navkey Route.ConfigurationAdd |> Effect.fromCmd )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Configurations"
    , attributes = []
    , element = viewContent model
    , route = Route.ConfigurationList
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    flatContainer s
        "Configurations"
        [ button.primary Add "Add..."
        ]
        none
        [ wrappedRow
            [ spacing 10 ]
            (s.state.configs
                |> Set.toList
                |> List.map
                    (\c ->
                        viewSmallCard (Removed c)
                            Nothing
                            (Configuration.View.display s c)
                            none
                    )
                |> withDefaultContent (p "There are no Configurations yet. Create your first one!")
            )
        ]