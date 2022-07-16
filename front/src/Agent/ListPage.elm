module Agent.ListPage exposing (match, page)

import Entity.ListPage exposing (Config, Flags, Model, Msg)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import View exposing (..)


config : Config
config =
    { pageTitle = "Agents"
    , entityType = "Agent"
    , emptyText = "There are no Agents yet. Add your first one!"
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = Entity.ListPage.init s
        , update = Entity.ListPage.update s
        , view = Entity.ListPage.view config s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.Agents ->
            Just { route = route }

        _ ->
            Nothing
