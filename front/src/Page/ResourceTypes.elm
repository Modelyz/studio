module Page.ResourceTypes exposing (match, page)

import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)
import View.EntityTypes exposing (Config, Flags, Model, Msg)


config : Config
config =
    { pageTitle = "Resource Types"
    , entityType = "ResourceType"
    , emptyText = "There are no ResourceTypes yet. Create your first one!"
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = View.EntityTypes.init s
        , update = View.EntityTypes.update s
        , view = View.EntityTypes.view config s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.ResourceTypes ->
            Just { route = route }

        _ ->
            Nothing
