module CommitmentType.ListPage exposing (match, page)

import CommitmentType.CommitmentType exposing (CommitmentType)
import DictSet as Set exposing (DictSet)
import Effect exposing (Effect)
import Element exposing (..)
import Item.Item as Item exposing (Item)
import Message exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect, redirectAdd)
import Search.Criteria as Criteria exposing (Criteria(..))
import Shared
import Spa.Page
import View exposing (..)
import View.Smallcard exposing (viewSmallCard)
import View.Type as ViewType


type alias Model =
    { route : Route
    , search : Criteria CommitmentType
    }


type Msg
    = Removed CommitmentType
    | Add
    | Search String


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
        Route.CommitmentTypeList ->
            Just { route = route }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route, search = SearchNothing }, closeMenu f s.menu )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Removed cmt ->
            ( model, Shared.dispatch s <| RemovedCommitmentType cmt )

        Add ->
            ( model, redirectAdd "add" s.navkey model.route |> Effect.fromCmd )

        Search str ->
            ( { model | search = SearchFull str }, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "CommitmentTypes"
    , attributes = []
    , element = viewContent model ViewType.Smallcard
    , route = model.route
    }


viewContent : Model -> ViewType.Type -> Shared.Model -> Element Msg
viewContent model vt s =
    --TODO |> Criteria.entitySearch model.search
    case vt of
        ViewType.Smallcard ->
            flatContainer s
                "CommitmentTypes"
                [ button.primary Add "Add..."
                ]
                none
                [ wrappedRow
                    [ spacing 10 ]
                    (s.state.commitmentTypes
                        |> Set.toList
                        |> List.map
                            (\cmt ->
                                viewSmallCard (Removed cmt)
                                    (text <| Uuid.toString cmt.uuid)
                                    (cmt.parent
                                        |> Maybe.andThen (Item.find s.state.commitmentTypes)
                                        |> Maybe.map
                                            (\cmtp -> row [] [ text "Type: ", text <| Uuid.toString cmtp.uuid ])
                                        |> Maybe.withDefault none
                                    )
                            )
                        |> withDefaultContent (p "There are no CommitmentTypes yet. Add your first one!")
                    )
                ]

        ViewType.New ->
            text "New"
