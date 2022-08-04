module AgentType.AddPage exposing (..)

import AgentType.AgentType exposing (AgentType)
import Entity.AddPage exposing (Flags, Model, Msg)
import Entity.Entity as Entity exposing (Entity, only)
import Entity.Type as Type exposing (Type)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


config : Entity.AddPage.Config
config =
    { filter = only "AgentType"
    , typeExplain = "Choose the type of the new Agent Type (it can be hierarchical)"
    , pageTitle = "Adding an Agent Type"
    , currentType = Type.AgentType
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
        Route.AgentTypeAdd ->
            Just { route = route }

        _ ->
            Nothing


validate : Model -> Result String Entity
validate m =
    case m.flatselect of
        Just (Entity.AT t) ->
            Ok <| Entity.AT <| AgentType m.uuid (Maybe.map Entity.toUuid m.flatselect) Nothing

        Just _ ->
            Err "You cannot have this type for this Entity"

        Nothing ->
            Ok <| Entity.AT <| AgentType m.uuid Nothing Nothing
