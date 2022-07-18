module CommitmentType.AddPage exposing (..)

import CommitmentType.CommitmentType exposing (CommitmentType)
import Entity.AddPage exposing (Flags, Model, Msg)
import Entity.Entity as Entity exposing (Entity, only)
import Entity.Type as Type exposing (Type)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


config : Entity.AddPage.Config CommitmentType
config =
    { filter = only "CommitmentType"
    , typeExplain = "Choose the type of the new Commitment Type (it can be hierarchical)"
    , pageTitle = "Adding a Commitment Type"
    , constructor = Entity.CmT
    , currentType = Type.CommitmentType
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
        Route.CommitmentTypeAdd ->
            Just { route = route }

        _ ->
            Nothing


validate : Model -> Result String Entity
validate m =
    case m.flatselect of
        Just (Entity.CmT t) ->
            Ok (Entity.CmT (CommitmentType m.uuid (m.flatselect |> Maybe.map Entity.toUuid)))

        Just _ ->
            Err "You cannot have this type for this Entity"

        Nothing ->
            Ok (Entity.CmT (CommitmentType m.uuid Nothing))
