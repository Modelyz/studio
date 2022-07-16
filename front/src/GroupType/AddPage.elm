module GroupType.AddPage exposing (..)

import EntityType.AddPage exposing (Flags, Model, Msg)
import EntityType.EntityType as EntityType exposing (EntityType(..), only)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


config : EntityType.AddPage.Config
config =
    { filter = only EntityType.GroupType
    , typeExplain = "Choose to restrict what you can group together"
    , nameExplain = "Give a name to this new Group Type"
    , pageTitle = "Adding an Group Type"
    , processRestriction = "This Group Type will be available from the following Process Types:"
    , typeConstructor = GroupType
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
        Route.GroupTypeAdd ->
            Just { route = route }

        _ ->
            Nothing
