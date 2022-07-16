module AgentType.AddPage exposing (..)

import EntityType.AddPage exposing (Flags, Model, Msg)
import EntityType.EntityType as EntityType exposing (EntityType, only)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


config : EntityType.AddPage.Config
config =
    { filter = only EntityType.AgentType
    , typeExplain = "Choose the type of the new Agent Type (it can be hierarchical)"
    , nameExplain = "Give a name to this new Agent Type"
    , pageTitle = "Adding an Agent Type"
    , processRestriction = "This Agent Type will be available from the following Process Types:"
    , typeConstructor = EntityType.AgentType
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
        Route.AgentTypeAdd ->
            Just { route = route }

        _ ->
            Nothing
