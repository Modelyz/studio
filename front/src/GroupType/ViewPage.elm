module GroupType.ViewPage exposing (Flags, Model, Msg(..), match, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Group.View exposing (displayGroupTable)
import GroupType.GroupType exposing (GroupType)
import Hierarchy.Type as HType
import Ident.Identifiable exposing (getIdentifiers)
import Ident.View exposing (displayIdentifierDict)
import Prng.Uuid as Uuid exposing (Uuid)
import Route exposing (Route, redirect)
import Shared
import Spa.Page
import Tree
import Type exposing (Type)
import Typed.Type as TType
import Util exposing (third)
import Value.Valuable exposing (getValues)
import Value.View exposing (displayValueDict)
import View exposing (..)
import Configuration.Zone.View exposing (displayZone)
import Configuration.Zone exposing (Zone(..))


mainHType : Type
mainHType =
    Type.HType HType.GroupType


type alias Flags =
    { route : Route
    , uuid : Uuid
    }


type alias Model =
    { route : Route
    , what : Type
    , uuid : Uuid
    , type_ : Maybe Uuid
    , groupType : Maybe GroupType
    , groups : List Uuid
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
        Route.Entity Route.GroupType (Route.View uuid _) ->
            Uuid.fromString uuid |> Maybe.map (Flags route)

        _ ->
            Nothing


init : Shared.Model -> Flags -> ( Model, Effect Shared.Msg Msg )
init s f =
    ( { route = f.route
      , what = mainHType
      , uuid = f.uuid
      , type_ = Maybe.andThen third (Dict.get (Uuid.toString f.uuid) s.state.types)
      , groupType = Dict.get (Uuid.toString f.uuid) s.state.groupTypes
      , groups =
            s.state.grouped
                |> Dict.filter (\_ link -> link.groupable == f.uuid)
                |> Dict.values
                |> List.map (\link -> link.group)
      }
    , closeMenu f s.menu
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update s msg model =
    case msg of
        Close ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.GroupType (Route.List (Maybe.map Uuid.toString model.type_)) )

        Edit ->
            ( model, Effect.fromCmd <| redirect s.navkey <| Route.Entity Route.GroupType <| Route.Edit (Uuid.toString model.uuid) Nothing )


view : Shared.Model -> Model -> View Msg
view s model =
    { title = "Adding a Group Type"
    , attributes = []
    , element = viewContent model
    , route = model.route
    }


viewContent : Model -> Shared.Model -> Element Msg
viewContent model s =
    model.groupType
        |> Maybe.map
            (\gt ->
                floatingContainer s
                    (Just Close)
                    "Group Type"
                    [ button.primary (Ok Edit) "Edit" ]
                    [ h2 "Identifiers:"
                    , text <| displayZone s.state SmallcardZone mainHType model.uuid
                    , getIdentifiers s.state model.what model.uuid model.type_ False
                        |> displayIdentifierDict "(none)"
                    , h2 "Type:"
                    , Dict.get (Uuid.toString model.uuid) s.state.types
                        |> Maybe.andThen (\( _, _, mpuuid ) -> Maybe.map (\puuid -> displayZone s.state SmallcardZone mainHType puuid) mpuuid)
                        |> Maybe.withDefault ""
                        |> text
                    , h2 "An entity can only be in one group of this type: "
                    , text <|
                        if gt.unique then
                            "True"

                        else
                            "False"
                    , h2 "Groups of this type are:"
                    , text <| Tree.toString gt.treeType
                    , h2 "Values:"
                    , getValues s.state.types s.state.valueTypes s.state.values model.what model.uuid model.type_ False
                        |> displayValueDict s { context = ( Type.HType HType.GroupType, model.uuid ) } "(none)" s.state.values
                    , h2 "Groups:"
                    , model.groups
                        |> List.map (\guuid -> displayZone s.state SmallcardZone (Type.TType TType.Group) guuid)
                        |> displayGroupTable "(none)"
                    ]
            )
        |> Maybe.withDefault (text "Not found")
