module ResourceType.ListPage exposing (match, page)

import EntityType.ListPage exposing (Config, Flags, Model, Msg)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


config : Config
config =
    { pageTitle = "Resource Types"
    , entityType = "ResourceType"
    , emptyText = "There are no Resource Types yet. Create your first one!"
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = EntityType.ListPage.init s
        , update = EntityType.ListPage.update s
        , view = EntityType.ListPage.view config s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.ResourceTypeList ->
            Just { route = route }

        _ ->
            Nothing
