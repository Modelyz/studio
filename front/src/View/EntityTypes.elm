module View.EntityTypes exposing (Config, Flags, Model, Msg, init, update, view)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Event
import REA.EntityType as ENT exposing (EntityType)
import Route exposing (Route, redirectAdd)
import Shared
import View exposing (..)


type alias Model =
    { route : Route }


type Msg
    = Removed EntityType
    | Added


type alias Flags =
    { route : Route }


type alias Config =
    { pageTitle : String
    , entityType : String
    , emptyText : String
    }


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route }, closeMenu s.menu )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed et ->
            ( model
            , Shared.dispatch s <| Event.TypeRemoved et
            )

        Added ->
            ( model, redirectAdd "add" s.navkey model.route |> Effect.fromCmd )


view : Config -> Shared.Model -> Model -> View Msg
view c s model =
    { title = c.pageTitle
    , attributes = []
    , element = viewContent c model
    , route = model.route
    }


viewContent : Config -> Model -> Shared.Model -> Element Msg
viewContent c model s =
    let
        entityTypes =
            Set.filter (\et -> ENT.toString et == c.entityType) s.state.entityTypes
    in
    flatContent s
        c.pageTitle
        [ button.primary Added "Add..."
        ]
        [ wrappedRow
            [ spacing 10 ]
            (entityTypes
                |> Set.toList
                |> List.map (\et -> viewSmallCard (Removed et) (ENT.toName et) <| Maybe.withDefault "(Root type)" <| Maybe.map (\t -> "Type: " ++ t) (ENT.toParent et))
                |> withDefaultContent (p c.emptyText)
            )
        ]
