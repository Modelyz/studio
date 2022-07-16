module AgentType.ViewPage exposing (match, page, view)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (Element, text)
import EntityType.EntityType as EntityType exposing (EntityType)
import Route exposing (Route, redirect, redirectAdd)
import Shared exposing (findEntityType)
import Spa.Page
import Url exposing (percentDecode)
import View exposing (View, closeMenu)


type alias Model =
    { route : Route
    , entityType : Maybe EntityType
    }


type Msg
    = Edit


type alias Flags =
    { route : Route, name : String }


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
        Route.AgentType name ->
            Just { route = route, name = name }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , entityType =
            Just f.name |> Maybe.andThen (\etname -> s.state.entityTypes |> EntityType.only EntityType.AgentType |> findEntityType etname)
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Edit ->
            ( model, redirectAdd "edit" s.navkey model.route |> Effect.fromCmd )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Agent Type"
    , attributes = []
    , element = \_ -> viewContent s model
    , route = model.route
    }


viewContent : Shared.Model -> Model -> Element Msg
viewContent s model =
    let
        identifiers =
            s.state.identifiers
    in
    model.entityType |> Maybe.map EntityType.toName |> Maybe.withDefault "not found" |> text



-- depuis l'et, retrouver la liste des identifier
--
