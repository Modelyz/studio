module Group.ListGroupTypePage exposing (match, page, view)

import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Event
import Group.GroupType as GroupType exposing (GroupType)
import Html.Attributes as Attr
import Prng.Uuid as Uuid
import REA.Entity as Entity exposing (Entity, toPluralString, toUuid)
import Result exposing (andThen)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import Style exposing (..)
import View exposing (..)
import View.Navbar as Navbar
import View.Radio as Radio


type alias Model =
    { route : Route }


type Msg
    = Removed GroupType
    | Add


type alias Flags =
    { route : Route }


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
        Route.GroupTypes ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route }, closeMenu f s.menu )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed g ->
            ( model
            , Shared.dispatch s <| Event.GroupTypeRemoved g.name
            )

        Add ->
            ( model, redirect s.navkey Route.AddGroupType |> Effect.fromCmd )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Group Types"
    , attributes = []
    , element = viewContent model
    , route = Route.GroupTypes
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    flatContent s
        "Group Types"
        [ button.primary Add "Add..."
        ]
        [ wrappedRow
            [ spacing 10 ]
            (s.state.grouptypes
                |> Set.toList
                |> List.sortBy .name
                |> List.map
                    (\g ->
                        viewSmallCard (Removed g)
                            Nothing
                            (text g.name)
                            ""
                    )
                |> withDefaultContent (p "There are no Group Types yet. Create your first one!")
            )
        ]
