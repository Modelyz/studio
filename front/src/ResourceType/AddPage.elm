module ResourceType.AddPage exposing (..)

import Entity.AddPage exposing (Flags, Model, Msg)
import Entity.Entity as Entity exposing (Entity, only)
import Entity.Type as Type
import ResourceType.ResourceType exposing (ResourceType)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


config : Entity.AddPage.Config ResourceType
config =
    { filter = only "ResourceType"
    , typeExplain = "Choose the type of the new Resource Type (it can be hierarchical)"
    , pageTitle = "Adding a Resource Type"
    , constructor = Entity.RT
    , currentType = Type.ResourceType
    , validate = validate
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
        Route.ResourceTypeAdd ->
            Just { route = route }

        _ ->
            Nothing


validate : Model -> Result String Entity
validate m =
    case m.flatselect of
        Just (Entity.RT t) ->
            Ok (Entity.RT (ResourceType m.uuid (m.flatselect |> Maybe.map Entity.toUuid)))

        Just _ ->
            Err "You cannot have this type for this Entity"

        Nothing ->
            Ok (Entity.RT (ResourceType m.uuid Nothing))
