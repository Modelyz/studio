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
import Value.HardLink as HardLink exposing (HardLink)
import Value.Value as Value exposing (DeepLink(..), Value, addTail)
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
    , stackNum : Int
    , targetPath : List Int
    }


type Msg
    = AddedHardlink HardLink
    | Cancel
    | Selected DeepLink


init : Shared.Model -> Scope -> Model
init s scope =
    -- scope is the scope of the Value being added
    { selection = OnlyScope scope
    , scope = scope
    , deeplink = Null
    , stackNum = 0
    , targetPath = []
    }


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update s msg model =
    case msg of
        Selected deeplink ->
            ( { model | selection = OnlyScope model.scope }, Cmd.none )

        Cancel ->
            ( { model | selection = OnlyScope model.scope }, Cmd.none )

        AddedHardlink hardLink ->
            ( { model | deeplink = addTail hardLink model.deeplink }, Cmd.none )


view : Shared.Model -> Model -> Element Msg
view s model =
    floatingContainer s
        (Just Cancel)
        "Building a link to another Value"
        [ wrappedRow [ width fill, spacing 20 ]
            [ button.secondary Cancel "Cancel"
            , button.disabled "Please select a value" "Choose"
            ]
        ]
    <|
        [ text <| Value.displayDeepLink model.deeplink
        , wrappedRow [ spacing 20 ] <|
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
                                            HardLink.allRL

                                        Type.TType TType.Commitment ->
                                            HardLink.allCmL

                                        -- FIXME
                                        _ ->
                                            []

                                _ ->
                                    []
                    in
                    List.map (\l -> button.primary (AddedHardlink l) (HardLink.hardlinkToString l)) allLink

                HardLinkAndScope scope name ->
                    []
        ]


selectHardLink : Shared.Model -> Model -> (String -> Msg) -> Element Msg
selectHardLink s model onInput =
    column [ spacing 20 ]
        [ h2 "Choose between:"
        ]
