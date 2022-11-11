module Value.DeepLink exposing (Model, Msg(..), init, update, view)

import Dict
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Hierarchy.Type as HType
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope(..))
import Scope.View exposing (selectScope)
import Shared
import Type exposing (Type)
import Typed.Type as TType
import Value.HardLink as HL exposing (HardLink)
import Value.Select as Select
import Value.Value as Value exposing (DeepLink(..), Value, addTail)
import View exposing (..)
import View.Smallcard exposing (clickableCard, viewHalfCard)
import View.Style exposing (..)


type alias Model =
    { scope : Scope
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
    { scope = scope
    , deeplink = Null
    , stackNum = 0
    , targetPath = []
    }


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update s msg model =
    case msg of
        Selected deeplink ->
            ( model, Cmd.none )

        Cancel ->
            ( model, Cmd.none )

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
        , h2 "Choose between:"
        , wrappedRow [ spacing 20 ] <|
            case model.deeplink of
                {- TODO si on est sur null, on a notre contexte qui est le
                   scope choisi pour la value en cours de création.  Dans ce cas
                   on affiche les all du commitment (on extrait le type du scope).
                   Le contexte est donc un scope. Quand on choisit un hardlink, on
                   modifie le scope contexte en conséquence (sauf que là on n'a
                   pas de uuid donc le scope est un HasType). Si dans la
                   configuration du commitment pour le lien receiver on a un
                   scope, alors on remplace le scope en contexte par celui là.
                   Puis on affiche les all du nouveau contexte. On affiche aussi
                   les values correspondant au nouveau scope en contexte.

                      si on est sur Link, on vient de choisir un hardlink, on a un
                      nouveau scope qui est hastype (si pas de config sur le
                      hardlink) ou le scope du hardlink.

                       si on est sur EndPoint, alors on a sélectionné une value,
                       avec laquelle on veut remplacer le null. Dans ce cas on
                       affiche le bouton Choose.
                -}
                Null ->
                    List.map (\l -> button.primary (AddedHardlink l) (HL.hardlinkToString l)) (scopeToChoice model.scope)

                Link hl dl ->
                    -- TODO replace Nothing with Just scope of the restricted scope of the hardlink
                    List.map (\l -> button.primary (AddedHardlink l) (HL.hardlinkToString l)) (HL.hlToChoice hl)

                EndPoint value ->
                    []
        ]
            ++ [ selectValue s model (dlToScope model model.deeplink) (Selected << EndPoint) ]


dlToScope : Model -> DeepLink -> Scope
dlToScope model deeplink =
    case deeplink of
        Null ->
            model.scope

        Link hl dl ->
            dlToScope model dl

        EndPoint v ->
            model.scope


selectValue : Shared.Model -> Model -> Scope -> (Value -> Msg) -> Element Msg
selectValue s model scope onInput =
    case scope of
        IsItem t uuid ->
            column [ spacing 20 ]
                [ h2 "Select the value you want to choose:"
                , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
                    (s.state.values
                        |> Dict.values
                        |> List.filter (\v -> v.what == t && v.for == uuid)
                        |> List.map (\v -> clickableCard (onInput v) (text v.name) none)
                        |> withDefaultContent (text "(No values are defined for this entity. Choose another entity or create a value for this entity)")
                    )
                ]

        _ ->
            none


scopeToChoice : Scope -> List HardLink
scopeToChoice scope =
    case scope of
        HasUserType t ht uuid ->
            case t of
                Type.TType TType.Resource ->
                    HL.allRL

                Type.TType TType.Event ->
                    HL.allEL

                Type.TType TType.Agent ->
                    HL.allAL

                Type.TType TType.Commitment ->
                    HL.allCmL

                Type.TType TType.Contract ->
                    HL.allCnL

                Type.TType TType.Process ->
                    HL.allPL

                Type.TType TType.Group ->
                    HL.allGL

                Type.HType HType.ResourceType ->
                    HL.allRL

                Type.HType HType.EventType ->
                    HL.allEL

                Type.HType HType.AgentType ->
                    HL.allAL

                Type.HType HType.CommitmentType ->
                    HL.allCmL

                Type.HType HType.ContractType ->
                    HL.allCnL

                Type.HType HType.ProcessType ->
                    HL.allPL

                Type.HType HType.GroupType ->
                    HL.allGL

        _ ->
            []
