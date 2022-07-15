module ProcessType.ListPage exposing (match, page)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Entity.Entity as Entity exposing (Entity, toPluralString)
import EntityType.EntityType as ENT exposing (EntityType)
import EntityType.ListPage exposing (Config, Flags, Model, Msg)
import Html.Attributes as Attr
import Navbar
import Result exposing (andThen)
import Route exposing (Route)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.Radio as Radio


config : Config
config =
    { pageTitle = "Process Types"
    , entityType = "ProcessType"
    , emptyText = "There are no Process Types yet. Create your first one!"
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = EntityType.ListPage.init s
        , update = EntityType.ListPage.update s
        , view = EntityType.ListPage.view config s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.ProcessTypes ->
            Just { route = route }

        _ ->
            Nothing
