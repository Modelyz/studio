module Contract.ViewPage exposing (Flags, Model, Msg(..), match, page)

import Contract.Contract exposing (Contract)
import ContractType.ContractType exposing (ContractType)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable
import Group.View exposing (displayGroupTable)
import Hierarchy.Hierarchic as H
import Ident.Identifiable exposing (gWithIdentifiers, hWithIdentifiers, tWithIdentifiers)
import Ident.View exposing (displayIdentifierDict)
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import Typed.Typed as T
import Value.Input exposing (inputValues)
import Value.Valuable exposing (tWithValues, withValues)
import Value.Value as Value exposing (Value)
import Value.ValueType exposing (initValues)
import Value.View exposing (displayValueDict)
import View exposing (..)
import Zone.View exposing (hWithDisplay, tWithDisplay)
import Zone.Zone exposing (Zone(..))


allT : Shared.Model -> Dict String Contract
allT =
    .state >> .contracts


allH : Shared.Model -> Dict String ContractType
allH =
    .state >> .contractTypes


type alias Flags =
    { route : Route
    , uuid : Maybe Uuid
    }


type alias Model =
    { route : Route
    , contract : Maybe Contract
    , groups : Dict String Group
    }


type Msg
    = Edit
    | Close


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
        Route.Entity Route.Contract (Route.View uuid) ->
            Just { route = route, uuid = Uuid.fromString uuid }

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    let
        mt =
            f.uuid |> Maybe.andThen (T.find (allT s))
    in
    ( { route = f.route
      , contract = mt
      , groups =
            mt
                |> Maybe.map
                    (\contract ->
                        s.state.grouped
                            |> Dict.filter (\_ v -> contract.uuid == Groupable.uuid v.groupable)
                            |> Dict.foldl (\_ v d -> Dict.insert (Group.compare v.group) v.group d) Dict.empty
                    )
                |> Maybe.withDefault Dict.empty
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Close ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.Contract <| Route.List Nothing )

        Edit ->
            model.contract
                |> Maybe.map
                    (\at ->
                        ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.Contract <| Route.Edit (Uuid.toString at.uuid) )
                    )
                |> Maybe.withDefault ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding a Contract Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    model.contract
        |> Maybe.map
            (\t ->
                floatingContainer s
                    (Just Close)
                    "Contract"
                    [ button.primary Edit "Edit" ]
                    [ h2 "Type:"
                    , t.type_
                        |> H.find (allH s)
                        |> Maybe.map (hWithIdentifiers (allT s) (allH s) s.state.identifierTypes s.state.identifiers)
                        |> Maybe.map (hWithDisplay (allT s) (allH s) s.state.configs SmallcardTitle)
                        |> Maybe.map .display
                        |> Maybe.andThen (Dict.get "SmallcardTitle")
                        |> Maybe.withDefault "(none)"
                        |> text
                    , h2 "Identifiers:"
                    , t
                        |> tWithIdentifiers (allT s) (allH s) s.state.identifierTypes s.state.identifiers
                        |> .identifiers
                        |> displayIdentifierDict "(none)"
                    , h2 "Values:"
                    , t
                        |> tWithValues s.state.contracts s.state.contractTypes s.state.valueTypes s.state.values
                        |> .values
                        |> displayValueDict "(none)" s.state.values
                    , h2 "Groups:"
                    , model.groups
                        |> Dict.values
                        |> List.map (gWithIdentifiers s.state.groups s.state.groupTypes s.state.identifierTypes s.state.identifiers)
                        |> List.map (tWithDisplay s.state.groups s.state.groupTypes s.state.configs SmallcardTitle)
                        |> List.map .display
                        |> List.map (Dict.get "SmallcardTitle" >> Maybe.withDefault "(missing zone config)")
                        |> displayGroupTable "(none)"
                    ]
            )
        |> Maybe.withDefault
            (floatingContainer s
                (Just Close)
                "Contract"
                []
                [ h1 "Not found", text "The current URL does not correspond to anything" ]
            )
