module Process.AddPage exposing (..)

import Entity.AddPage exposing (Flags, Model, Msg)
import Entity.Entity as Entity exposing (Entity, only)
import Process.Process exposing (Process)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


config : Entity.AddPage.Config Process
config =
    { filter = only "ProcessType"
    , typeExplain = "Choose the type of the new Process (it can be hierarchical)"
    , pageTitle = "Adding a Process"
    , constructor = Entity.A
    , typeName = "ProcessType"
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
        Route.ProcessAdd ->
            Just { route = route }

        _ ->
            Nothing
