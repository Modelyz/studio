module Configuration.Menu.ListPage exposing (Flags, Model, Msg, match, page)

{-
   Draft page for an easy menu configuration : all types, and checkboxes on each one to display or hide in the menu
-}

import Configuration exposing (Configuration)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Message
import Prng.Uuid exposing (Uuid)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (..)
import View.Smallcard exposing (clickableRemovableCard)


type alias Model =
    { route : Route }


type Msg
    = SetInvisible Configuration
    | SetVisible Configuration


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
update _ _ model =
    ( model, Effect.none )


view : Model -> View Msg
view model =
    { title = "Menu Items"
    , attributes = []
    , element = viewContent
    , route = model.route
    }


toTuples : Dict String { uuid : Uuid, what : Type } -> Dict String ( Type, Uuid )
toTuples entities =
    Dict.values entities
        |> List.map
            (\entity ->
                ( entity.uuid, entity.what )
            )


viewContent : Shared.Model -> Element Msg
viewContent s =
    let
        allTypes =
            [ ( "Resource Types", toTuples s.state.resourceTypes )
            , ( "Event Types", toTuples s.state.eventTypes )
            , ( "Agent Types", toTuples s.state.agentTypes )
            , ( "Commitment Types", toTuples s.state.commitmentTypes )
            , ( "Contract Types", toTuples s.state.contractTypes )
            , ( "Group Types", toTuples s.state.groupTypes )
            , ( "Process Types", toTuples s.state.processTypes )
            ]
    in
    flatContainer s
        Nothing
        "Configurations"
        [ button.primary (Ok SetVisible) "SetVisible..."
        ]
        none
        none
        [ p "Choose which entity types will appear in the menu"
        , wrappedRow
            [ spacing 10 ]
            (allTypes
                |> List.map
                    (\( title, entities ) ->
                        column []
                            [ h1 title
                            , entities
                                |> Dict.values
                                |> List.map
                                    (\c ->
                                        clickableRemovableCard (View <| Configuration.compare c)
                                            (SetInvisible c)
                                            (text <| Configuration.View.description s c)
                                            (text <| Configuration.View.view c)
                                    )
                                |> withDefaultContent (p "There are no Configurations yet. Create your first one!")
                            ]
                    )
            )
        ]
