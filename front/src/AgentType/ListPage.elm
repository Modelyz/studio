module AgentType.ListPage exposing (match, page)

import Entity.ListPage exposing (Config, Flags, Model, Msg)
import Route exposing (Route)
import Shared
import Entity.ListPage
import Entity.Entity as Entity exposing (Entity)
import Spa.Page
import View exposing (View)


config : Config
config =
    { pageTitle = "Agent Types"
    , entityType = "AgentType"
    , emptyText = "There are no Agent Types yet. Create your first one!"
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
        Route.AgentTypeList ->
            Just { route = route }

        _ ->
            Nothing
