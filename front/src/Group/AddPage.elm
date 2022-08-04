module Group.AddPage exposing (..)

import Entity.AddPage exposing (Flags, Model, Msg)
import Entity.Entity as Entity exposing (Entity(..), only)
import Entity.Type as Type exposing (Type)
import Group.Definition exposing (Definition(..))
import Group.Group exposing (Group)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


config : Entity.AddPage.Config
config =
    { filter = only "GroupType"
    , typeExplain = "Choose the type of the new Group (it can be hierarchical)"
    , pageTitle = "Adding a Group"
    , currentType = Type.Group
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
        Route.GroupAdd ->
            Just { route = route }

        _ ->
            Nothing


validate : Model -> Result String Entity
validate m =
    case m.flatselect of
        Just (Entity.GT t) ->
            Ok (Entity.G (Group m.uuid t.uuid Empty))

        Just _ ->
            Err "You cannot have this type for this Entity"

        Nothing ->
            Err "You must select a type"
