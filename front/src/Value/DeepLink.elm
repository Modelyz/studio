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
            ++ [ selectValue s model (Debug.log "Scope" (dlToScope model.scope (Debug.log "deeplink" model.deeplink))) (Selected << EndPoint) ]


dlToScope : Scope -> DeepLink -> Scope
dlToScope scope deeplink =
    -- TODO pass the restriction scope on the hardlink to narrow the choice of values
    case deeplink of
        Null ->
            scope

        Link hl dl ->
            dlToScope (hlToScope hl) dl

        EndPoint v ->
            scope


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
                        |> withDefaultContent (text "(No values are defined for this scope. Choose another entity or create a value for this entity)")
                    )
                ]

        HasUserType t ht _ ->
            column [ spacing 20 ]
                [ h2 "Select the value you want to choose:"
                , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
                    (s.state.values
                        |> Dict.values
                        |> List.filter (\v -> v.what == Type.HType ht)
                        |> List.map (\v -> clickableCard (onInput v) (text v.name) none)
                        |> withDefaultContent (text "(No values are defined for this scope. Choose another entity or create a value for this entity)")
                    )
                ]

        HasType t ->
            column [ spacing 20 ]
                [ h2 "Select the value you want to choose:"
                , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
                    (s.state.values
                        |> Dict.values
                        |> List.filter (\v -> v.what == t)
                        |> List.map (\v -> clickableCard (onInput v) (text v.name) none)
                        |> withDefaultContent (text "(No values are defined for this scope. Choose another entity or create a value for this entity)")
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


hlToScope : HardLink -> Scope
hlToScope x =
    case x of
        HL.ResourceLink y ->
            rlToScope y

        HL.EventLink y ->
            elToScope y

        HL.AgentLink y ->
            alToScope y

        HL.CommitmentLink y ->
            cmlToScope y

        HL.ContractLink y ->
            cnlToScope y

        HL.ProcessLink y ->
            plToScope y

        HL.GroupLink y ->
            glToScope y

        HL.ResourceTypeLink y ->
            rtlToScope y

        HL.EventTypeLink y ->
            etlToScope y

        HL.AgentTypeLink y ->
            atlToScope y

        HL.CommitmentTypeLink y ->
            cmtlToScope y

        HL.ContractTypeLink y ->
            cntlToScope y

        HL.ProcessTypeLink y ->
            ptlToScope y

        HL.GroupTypeLink y ->
            gtlToScope y


rlToScope : HL.ResourceLink -> Scope
rlToScope x =
    case x of
        HL.ResourceGroup ->
            HasType (Type.TType TType.Group)

        HL.ResourceType ->
            HasType (Type.HType HType.ResourceType)


elToScope : HL.EventLink -> Scope
elToScope x =
    case x of
        HL.EventProvider ->
            HasType (Type.TType TType.Agent)

        HL.EventReceiver ->
            HasType (Type.TType TType.Agent)

        HL.EventInflow ->
            HasType (Type.TType TType.Resource)

        HL.EventOutflow ->
            HasType (Type.TType TType.Resource)

        HL.EventGroup ->
            HasType (Type.TType TType.Group)

        HL.EventType ->
            HasType (Type.HType HType.EventType)


alToScope : HL.AgentLink -> Scope
alToScope x =
    case x of
        HL.AgentGroup ->
            HasType (Type.TType TType.Group)

        HL.AgentType ->
            HasType (Type.HType HType.AgentType)


cmlToScope : HL.CommitmentLink -> Scope
cmlToScope x =
    case x of
        HL.CommitmentProvider ->
            HasType (Type.TType TType.Agent)

        HL.CommitmentReceiver ->
            HasType (Type.TType TType.Agent)

        HL.CommitmentInflow ->
            HasType (Type.TType TType.Resource)

        HL.CommitmentOutflow ->
            HasType (Type.TType TType.Resource)

        HL.CommitmentGroup ->
            HasType (Type.TType TType.Group)

        HL.CommitmentType ->
            HasType (Type.HType HType.CommitmentType)


cnlToScope : HL.ContractLink -> Scope
cnlToScope x =
    case x of
        HL.ContractGroup ->
            HasType (Type.TType TType.Group)

        HL.ContractType ->
            HasType (Type.HType HType.ContractType)


plToScope : HL.ProcessLink -> Scope
plToScope x =
    case x of
        HL.ProcessGroup ->
            HasType (Type.TType TType.Group)

        HL.ProcessType ->
            HasType (Type.HType HType.ProcessType)


glToScope : HL.GroupLink -> Scope
glToScope x =
    case x of
        HL.GroupGroup ->
            HasType (Type.TType TType.Group)

        HL.GroupType ->
            HasType (Type.HType HType.GroupType)


rtlToScope : HL.ResourceTypeLink -> Scope
rtlToScope x =
    case x of
        HL.ResourceTypeGroup ->
            HasType (Type.TType TType.Group)

        HL.ResourceTypeParent ->
            HasType (Type.HType HType.ResourceType)


etlToScope : HL.EventTypeLink -> Scope
etlToScope x =
    case x of
        HL.EventTypeProvider ->
            HasType (Type.TType TType.Agent)

        HL.EventTypeReceiver ->
            HasType (Type.TType TType.Agent)

        HL.EventTypeInflow ->
            -- TODO an inflow can also defined by be a RT
            HasType (Type.TType TType.Resource)

        HL.EventTypeOutflow ->
            HasType (Type.TType TType.Resource)

        HL.EventTypeGroup ->
            HasType (Type.TType TType.Group)

        HL.EventTypeParent ->
            HasType (Type.HType HType.EventType)


atlToScope : HL.AgentTypeLink -> Scope
atlToScope x =
    case x of
        HL.AgentTypeGroup ->
            HasType (Type.TType TType.Group)

        HL.AgentTypeParent ->
            HasType (Type.HType HType.AgentType)


cmtlToScope : HL.CommitmentTypeLink -> Scope
cmtlToScope x =
    case x of
        HL.CommitmentTypeProvider ->
            HasType (Type.TType TType.Agent)

        HL.CommitmentTypeReceiver ->
            HasType (Type.TType TType.Agent)

        HL.CommitmentTypeInflow ->
            HasType (Type.TType TType.Resource)

        HL.CommitmentTypeOutflow ->
            HasType (Type.TType TType.Resource)

        HL.CommitmentTypeGroup ->
            HasType (Type.TType TType.Group)

        HL.CommitmentTypeParent ->
            HasType (Type.HType HType.CommitmentType)


cntlToScope : HL.ContractTypeLink -> Scope
cntlToScope x =
    case x of
        HL.ContractTypeGroup ->
            HasType (Type.TType TType.Group)

        HL.ContractTypeParent ->
            HasType (Type.HType HType.ContractType)


ptlToScope : HL.ProcessTypeLink -> Scope
ptlToScope x =
    case x of
        HL.ProcessTypeGroup ->
            HasType (Type.TType TType.Group)

        HL.ProcessTypeParent ->
            HasType (Type.HType HType.ProcessType)


gtlToScope : HL.GroupTypeLink -> Scope
gtlToScope x =
    case x of
        HL.GroupTypeGroup ->
            HasType (Type.TType TType.Group)

        HL.GroupTypeParent ->
            HasType (Type.HType HType.GroupType)
