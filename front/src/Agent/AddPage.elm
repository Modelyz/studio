module Agent.AddPage exposing (..)

import Agent.Agent exposing (Agent)
import Entity.AddPage exposing (Flags, Model, Msg)
import Entity.Entity as Entity exposing (Entity, only)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


config : Entity.AddPage.Config Agent
config =
    { filter = only "AgentType"
    , typeExplain = "Choose the type of the new Agent (it can be hierarchical)"
    , pageTitle = "Adding a Agent"
    , constructor = Entity.A
    , typeName = "AgentType"
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = Entity.AddPage.init s
        , update = Entity.AddPage.update config s
        , view = Entity.AddPage.view config s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.AgentAdd ->
            Just { route = route }

        _ ->
            Nothing
