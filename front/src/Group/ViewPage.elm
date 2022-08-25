module Group.ViewPage exposing (..)

import Configuration as Config
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable exposing (Groupable)
import Group.Input exposing (inputGroups)
import Hierarchy.Hierarchic as H exposing (Hierarchic)
import Hierarchy.Type as HType
import Hierarchy.View exposing (toDesc)
import Ident.Identifiable as Identifiable exposing (withIdentifiers)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType exposing (initIdentifiers)
import Ident.Input exposing (inputIdentifiers)
import Ident.View exposing (displayIdentifierDict)
import Item.Item as Item exposing (Item)
import Json.Decode as Decode
import Message
import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended as Random exposing (Seed, initialSeed)
import Route exposing (Route, redirect, redirectParent)
import Scope.Scope as Scope exposing (Scope(..))
import Shared
import Spa.Page
import State exposing (State)
import Type exposing (Type)
import Typed.Type as TType
import Typed.Typed as T
import View exposing (..)
import View.Smallcard exposing (hClickableCard, hViewHalfCard, hViewSmallCard)
import View.Step as Step exposing (Step(..), buttons, isLast)
import View.Style exposing (..)
import Zone.Zone exposing (Zone(..))


type alias Flags =
    { route : Route
    , uuid : Maybe Uuid
    }


type alias Model =
    { route : Route
    , group : Maybe Group
    }


type Msg
    = Edit


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init s
        , update = update s
        , view = view s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.GroupView uuid ->
            Just { route = route, uuid = Uuid.fromString uuid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , group = f.uuid |> Maybe.andThen (T.find s.state.groups)
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Edit ->
            model.group
                |> Maybe.map
                    (\at ->
                        ( model, Effect.fromCmd <| redirect s.navkey (Route.GroupEdit (Uuid.toString at.uuid)) )
                    )
                |> Maybe.withDefault ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding a Group Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    model.group
        |> Maybe.map
            (\a ->
                let
                    mconfig =
                        model.group
                            |> Maybe.map .uuid
                            |> Maybe.andThen
                                (\uuid ->
                                    Config.getMostSpecific s.state.groups s.state.groupTypes s.state.configs SmallcardTitle (HasUserType (Type.TType TType.Group) uuid)
                                )
                in
                floatingContainer s
                    "Group"
                    [ button.primary Edit "Edit" ]
                    [ h2 "Parent type:"
                    , H.find s.state.groupTypes a.type_
                        |> Maybe.map (\p -> withIdentifiers s.state.identifiers p.what p.uuid p)
                        |> Maybe.map (\pat -> Identifiable.display mconfig pat)
                        |> Maybe.withDefault "(none)"
                        |> text
                    , h2 "Identifiers:"
                    , a
                        |> withIdentifiers s.state.identifiers a.what a.uuid
                        |> .identifiers
                        |> displayIdentifierDict "(none)"
                    ]
            )
        |> Maybe.withDefault
            (floatingContainer s
                "Group"
                []
                [ h1 "Not found", text "The URL you entered does not correspond to anything" ]
            )
