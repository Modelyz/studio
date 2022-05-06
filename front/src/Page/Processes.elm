module Page.Processes exposing (Model, match, page, view)

import DictSet
import Effect exposing (Effect)
import Element exposing (..)
import Element.Input as Input
import Event
import IOStatus exposing (IOStatus(..))
import Page.Navbar as Navbar
import Prng.Uuid as Uuid
import REA.Process as P exposing (Process)
import REA.ProcessType exposing (ProcessType)
import Route exposing (Route)
import Shared
import Spa.Page
import View exposing (View)


type alias Model =
    { ptype : ProcessType
    }


type Msg
    = NewProcess ProcessType


type alias Flags =
    ()


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page s =
    Spa.Page.element
        { init = init
        , update = update s
        , view = view s
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.Processes _ ->
            Just ()

        _ ->
            Nothing


init : Flags -> ( Model, Effect Shared.Msg Msg )
init flags =
    ( { ptype = ProcessType ""
      }
    , Effect.none
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        NewProcess ptype ->
            ( model
            , Shared.dispatch s <| Event.ProcessAdded { name = "new process", type_ = ptype.name }
            )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Processes"
    , attributes = []
    , element = viewContent s model
    }


viewContent : Shared.Model -> Model -> Element Msg
viewContent s model =
    row []
        [ Input.button []
            { onPress = Just (NewProcess model.ptype)
            , label = text ("New " ++ model.ptype.name)
            }
        , row []
            (DictSet.filter (\p -> p.type_ == model.ptype.name) s.state.processes
                |> DictSet.toList
                |> List.sortBy P.compare
                |> List.reverse
                |> List.map viewThumbnail
            )
        ]


viewThumbnail : Process -> Element Msg
viewThumbnail p =
    row []
        [ link []
            { url = "/process/" ++ Uuid.toString p.uuid, label = text <| "process " ++ Uuid.toString p.uuid }
        ]
