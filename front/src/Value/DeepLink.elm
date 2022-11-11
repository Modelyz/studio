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


selectHardLink : Shared.Model -> Model -> (String -> Msg) -> Element Msg
selectHardLink s model onInput =
    column [ spacing 20 ]
        [ h2 "Choose between:"
        ]


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
