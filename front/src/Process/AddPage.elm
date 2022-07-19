module Process.AddPage exposing (..)

import Entity.AddPage exposing (Flags, Model, Msg)
import Entity.Entity as Entity exposing (Entity, only)
import Entity.Type as Type exposing (Type)
import Process.Process exposing (Process)
import Route exposing (Route)
import Shared
import Spa.Page
import Time exposing (millisToPosix)
import View exposing (View)


config : Entity.AddPage.Config
config =
    { filter = only "ProcessType"
    , typeExplain = "Choose the type of the new Process (it can be hierarchical)"
    , pageTitle = "Adding a Process"
    , currentType = Type.Process
    , validate = validate
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = Entity.AddPage.init config s
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


validate : Model -> Result String Entity
validate m =
    case m.flatselect of
        Just (Entity.PT t) ->
            Ok (Entity.P (Process m.uuid t.uuid (millisToPosix 0)))

        Just _ ->
            Err "You cannot have this type for this Entity"

        Nothing ->
            Err "You must select a type"
