module Page.AddAgentType exposing (..)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Event
import Html.Attributes as Attr
import REA.EntityType as ENT exposing (EntityType(..))
import Result exposing (andThen)
import Route exposing (Route)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.AddEntityType exposing (Flags, Model, Msg(..), Step(..), validate)
import View.FlatSelect exposing (flatselect)
import View.Navbar as Navbar
import View.Radio as Radio
import View.Step as Step exposing (isFirst, nextOrValidate, nextStep, previousStep)


config : View.AddEntityType.Config
config =
    { typeExplain = "Choose the type of the new Agent Type (it can be hierarchical)"
    , nameExplain = "Give a name to this new Agent Type"
    , pageTitle = "Adding an Agent Type"
    , validate = validate
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
        Route.AddAgentType ->
            Just { route = route }

        _ ->
            Nothing
