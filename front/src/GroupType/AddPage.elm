module GroupType.AddPage exposing (..)

import Entity.AddPage exposing (Flags, Model, Msg)
import Entity.Entity as Entity exposing (only)
import GroupType.GroupType exposing (GroupType)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


config : Entity.AddPage.Config GroupType
config =
    { filter = only "GroupType"
    , typeExplain = "Choose to restrict what you can group together"
    , pageTitle = "Adding an Group Type"
    , constructor = Entity.GT
    , typeName = "GroupType"
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
        Route.GroupTypeAdd ->
            Just { route = route }

        _ ->
            Nothing
