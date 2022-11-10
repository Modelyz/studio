module Value.DeepLink exposing (Model, Msg(..), init, update, view)

import Dict
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope(..))
import Scope.View exposing (selectScope)
import Shared
import Type exposing (Type)
import Typed.Type as TType
import Value.Value as Value exposing (DeepLink(..), HardLink(..), allRL)
import View exposing (..)
import View.Smallcard exposing (clickableCard, viewHalfCard)
import View.Style exposing (..)


type Selection
    = OnlyScope Scope
    | HardLinkAndScope HardLink Scope


type alias Model =
    { selection : Selection
    , scope : Scope
    , deeplink : DeepLink
    , onSelect : DeepLink -> Msg
    , stackNum : Int
    , targetPath : List Int
    }


type Msg
    = InputHardlink HardLink
    | Cancel
    | Selected DeepLink


init : Shared.Model -> Scope -> Model
init s scope =
    -- scope is the scope of the Value being added
    { selection = OnlyScope scope
    , scope = scope
    , deeplink = EndPoint
    , onSelect = Selected
    , stackNum = 0
    , targetPath = []
    }


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update s msg model =
    case msg of
        InputHardlink deeplink ->
            ( { model | selection = OnlyScope model.scope }, Cmd.none )

        Cancel ->
            ( { model | selection = OnlyScope model.scope }, Cmd.none )

        Selected vs ->
            case vs of
                EndPoint ->
                    ( model, Cmd.none )

                Link hl dl ->
                    ( model, Cmd.none )


view : Shared.Model -> Model -> Element Msg
view s model =
    floatingContainer s
        (Just Cancel)
        "Building a link to another Value"
        [ wrappedRow [ width fill, spacing 20 ]
            [ button.secondary Cancel "Cancel"
            , case model.selection of
                HardLinkAndScope hardlink (IsItem type_ uuid) ->
                    button.primary (model.onSelect <| Link hardlink model.deeplink) "Choose"

                _ ->
                    button.disabled "Please select a value" "Choose"
            ]
        ]
    <|
        [ wrappedRow [ spacing 20 ] <|
            case model.selection of
                OnlyScope scope ->
                    let
                        r =
                            Debug.log "scope" scope

                        allLink =
                            case scope of
                                HasUserType t ht uuid ->
                                    case t of
                                        Type.TType TType.Resource ->
                                            Value.allRL

                                        Type.TType TType.Commitment ->
                                            Value.allCmL

                                        -- FIXME
                                        _ ->
                                            []

                                _ ->
                                    []
                    in
                    List.map (\l -> button.primary (model.onSelect <| Link model.deeplink) (Value.enlToString l)) allLink

                HardLinkAndScope scope name ->
                    []
        ]


selectHardLink : Shared.Model -> Model -> (String -> Msg) -> Element Msg
selectHardLink s model onInput =
    column [ spacing 20 ]
        [ h2 "Choose between:"
        ]
