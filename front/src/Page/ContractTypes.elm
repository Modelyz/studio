module Page.ContractTypes exposing (match, page)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import REA.ContractType as AT exposing (..)
import REA.Entity as Entity exposing (Entity, toPluralString)
import REA.EntityType as ENT exposing (EntityType)
import Result exposing (andThen)
import Route exposing (Route)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.EntityTypes exposing (Config, Flags, Model, Msg)
import View.Navbar as Navbar
import View.Radio as Radio


config : Config
config =
    { pageTitle = "Contract Types"
    , entityType = "ContractType"
    , emptyText = "There are no Contract Types yet. Create your first one!"
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = View.EntityTypes.init s
        , update = View.EntityTypes.update s
        , view = View.EntityTypes.view config s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.ContractTypes ->
            Just { route = route }

        _ ->
            Nothing
