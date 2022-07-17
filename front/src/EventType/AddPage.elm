module EventType.AddPage exposing (..)

import Entity.AddPage exposing (Flags, Model, Msg)
import Entity.Entity as Entity exposing (Entity, only)
import EventType.EventType exposing (EventType)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


config : Entity.AddPage.Config EventType
config =
    { filter = only "EventType"
    , typeExplain = "Choose the type of the new Event Type (it can be hierarchical)"
    , pageTitle = "Adding an Event Type"
    , constructor = Entity.ET
    , typeName = "EventType"
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
        Route.EventTypeAdd ->
            Just { route = route }

        _ ->
            Nothing
