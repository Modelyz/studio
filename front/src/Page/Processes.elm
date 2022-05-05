module Page.Processes exposing (Model, match, page, view)

import DictSet
import Effect exposing (Effect)
import Event
import Html exposing (Html, a, br, button, div, text)
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
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
    { route : Route
    , ptype : ProcessType
    }


type Msg
    = NewProcess ProcessType


type alias Flags =
    { route : Route
    }


page : Shared.Model -> Spa.Page.Page Flags Shared.Msg (View Msg) Model Msg
page shared =
    Spa.Page.element
        { init = init
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


match : Route -> Maybe Flags
match route =
    case route of
        Route.Processes _ ->
            Just { route = route }

        _ ->
            Nothing


init : Flags -> ( Model, Effect Shared.Msg Msg )
init flags =
    ( { route = flags.route
      , ptype = ProcessType ""
      }
    , Effect.none
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update shared msg model =
    case msg of
        NewProcess ptype ->
            ( model
            , Shared.dispatch shared <| Event.ProcessAdded { name = "new process", type_ = ptype.name }
            )


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Processes"
    , attributes = []
    , element =
        Html.div []
            [ Navbar.view shared model.route
            , viewContent shared model
            ]
    }


viewContent : Shared.Model -> Model -> Html Msg
viewContent shared model =
    div
        [ class "section"
        ]
        [ button
            [ onClick (NewProcess model.ptype)
            , class "button"
            ]
            [ text <| "New " ++ model.ptype.name
            ]
        , div [ class "columns is-multiline" ]
            (DictSet.filter (\p -> p.type_ == model.ptype.name) shared.processes
                |> DictSet.toList
                |> List.sortBy P.compare
                |> List.reverse
                |> List.map viewThumbnail
            )
        ]


viewThumbnail : Process -> Html Msg
viewThumbnail p =
    div [ class "column is-one-quarter" ]
        [ a [ href <| "/process/" ++ Uuid.toString p.uuid ]
            [ div [ class "box", id <| Uuid.toString p.uuid ]
                [ text "process"
                , br [] []
                , text <| Uuid.toString p.uuid
                ]
            ]
        ]
