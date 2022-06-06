module Page.AddCommitmentType exposing (..)

import REA.EntityType as ENT exposing (onlyType)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)
import View.AddEntityType exposing (Flags, Model, Msg)


config : View.AddEntityType.Config
config =
    { filter = onlyType "CommitmentType"
    , typeExplain = "Choose the type of the new Commitment Type (it can be hierarchical)"
    , nameExplain = "Give a name to this new Commitment Type"
    , pageTitle = "Adding a Commitment Type"
    , processRestriction = "This Commitment Type will be usable from the following Process Types:"
    , typeConstructor = ENT.CommitmentType
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = View.AddEntityType.init s
        , update = View.AddEntityType.update config s
        , view = View.AddEntityType.view config s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.AddCommitmentType ->
            Just { route = route }

        _ ->
            Nothing
