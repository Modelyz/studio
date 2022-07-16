module ResourceType.AddPage exposing (..)

import EntityType.AddPage exposing (Flags, Model, Msg)
import EntityType.EntityType as EntityType exposing (only)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


config : EntityType.AddPage.Config
config =
    { filter = only EntityType.ResourceType
    , typeExplain = "Choose the type of the new Resource Type (it can be hierarchical)"
    , nameExplain = "Give a name to this new Resource Type"
    , pageTitle = "Adding a Resource Type"
    , processRestriction = "This Resource Type will be usable from the following Process Types:"
    , typeConstructor = EntityType.ResourceType
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = EntityType.AddPage.init s
        , update = EntityType.AddPage.update config s
        , view = EntityType.AddPage.view config s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.ResourceTypeAdd ->
            Just { route = route }

        _ ->
            Nothing
