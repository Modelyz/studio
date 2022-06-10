module Page.Processes exposing (Model, match, page, view)

import DictSet
import Effect exposing (Effect)
import Element exposing (..)
import Element.Input as Input
import Event
import IOStatus exposing (IOStatus(..))
import Prng.Uuid as Uuid
import REA.EntityType as ENT exposing (EntityType)
import REA.Process as P exposing (Process)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View, closeMenu)
import View.Navbar as Navbar


type alias Model =
    { route : Route
    , ptype : EntityType
    }


type Msg
    = NewProcess EntityType


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
        Route.Processes _ ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , ptype = ENT.ProcessType { name = "", parent = Nothing }
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        NewProcess ptype ->
            ( model
            , Shared.dispatchT s <| \uuid t -> Event.ProcessAdded (Process uuid (ENT.toName ptype) t)
            )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Processes"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    column []
        [ Input.button []
            { onPress = Just (NewProcess model.ptype)
            , label = text ("New " ++ ENT.toName model.ptype)
            }
        , column []
            (DictSet.filter (\p -> p.type_ == ENT.toName model.ptype) s.state.processes
                |> DictSet.toList
                |> List.sortBy P.compare
                |> List.reverse
                |> List.map viewThumbnail
            )
        ]


viewThumbnail : Process -> Element Msg
viewThumbnail p =
    column []
        [ link []
            { url = "/process/" ++ Uuid.toString p.uuid, label = text <| "process " ++ Uuid.toString p.uuid }
        ]
