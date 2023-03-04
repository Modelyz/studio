module Group.Input exposing (Config, Model, Msg, added, init, inputGroups, removed, update)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Border as Border
import GroupType.GroupType as GroupType exposing (GroupType)
import Hierarchy.Type as HType
import Prng.Uuid as Uuid exposing (Uuid)
import Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Shared
import Tree
import Type
import Typed.Type as TType
import Util exposing (third)
import View exposing (..)
import View.Smallcard exposing (tClickableCard, viewHalfCard)
import View.Style exposing (..)
import Zone.View exposing (displayZone)
import Zone.Zone exposing (Zone(..))


type alias Config =
    { type_ : Type.Type
    , mpuuid : Maybe Uuid
    }


type alias Model =
    { oldGroups : Dict String Uuid

    -- gt uuid as keys, (gt, group uuids) as values
    , groups : Dict String ( GroupType, Dict String Uuid )

    -- currently edited list of groups of this type
    , currentGt : Maybe GroupType

    -- currently edited group
    , currentG : Maybe Uuid
    }



-- cux où on peut appuyer une seule fois sont les anciens groupes, qui ne sont pas hierarchiques.
-- ceux où on peut appuyer plusieurs fois sont les hiérarchiques.
-- mais on appuie une seule fois : ça permet de choisir le corps,
{-
      si Flat : on ne peut appuyer qu'une fois
      si Node : on peut appuyer une seule fois, et une card supplémentaire s'affiche pour sélectionner le noeud en cours (en disant Unknown
      Si Leaf : on peut appuyer une seule fois, et il faut aller jusqu'à la feuille pour finir l'ajout du groupe.
      Ensuite en fonction du unique=True ou False, on peut ajouter plusieurs groupes sur chaque ligne de type de groupe.
   Si unique, la liste de choix ne s'affiche pas s'il y a déjà un groupe.

-}


type Msg
    = SetCurrentGT GroupType
    | AddedGroup GroupType Uuid
    | RemovedGroup GroupType Uuid


groupDict : Model -> Dict String Uuid
groupDict model =
    model.groups
        |> Dict.values
        |> List.map (Tuple.second >> Dict.values)
        |> List.concat
        |> List.map (\uuid -> ( Uuid.toString uuid, uuid ))
        |> Dict.fromList


added : Model -> Dict String Uuid
added model =
    Dict.diff (groupDict model) model.oldGroups


removed : Model -> Dict String Uuid
removed model =
    Dict.diff model.oldGroups (groupDict model)


init : Shared.Model -> Dict String Uuid -> ( Model, Cmd Msg )
init s gdict =
    let
        gs =
            -- inserting a dict inside a dict...
            Dict.foldl
                (\_ uuid dict ->
                    Dict.get (Uuid.toString uuid) s.state.types
                        |> Maybe.andThen third
                        |> Maybe.andThen (\gtuuid -> Dict.get (Uuid.toString gtuuid) s.state.groupTypes)
                        |> Maybe.map
                            (\gt ->
                                let
                                    gset =
                                        Dict.get (Uuid.toString gt.uuid) dict
                                            |> Maybe.map Tuple.second
                                            |> Maybe.withDefault Dict.empty
                                in
                                Dict.insert
                                    (Uuid.toString gt.uuid)
                                    ( gt, Dict.insert (Uuid.toString uuid) uuid gset )
                                    dict
                            )
                        |> Maybe.withDefault dict
                )
                Dict.empty
                gdict
    in
    ( { groups = gs
      , currentGt = Nothing
      , currentG = Nothing
      , oldGroups = gdict
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        SetCurrentGT gt ->
            ( { model
                | currentGt = Just gt
                , groups =
                    Dict.union
                        (Dict.filter (\_ v -> not <| Dict.isEmpty (Tuple.second v)) model.groups)
                        (Dict.singleton (GroupType.compare gt) ( gt, Dict.empty ))
              }
            , Cmd.none
            )

        AddedGroup gt uuid ->
            let
                gset =
                    if gt.unique then
                        Dict.singleton (Uuid.toString uuid) uuid

                    else
                        Dict.get (Uuid.toString gt.uuid) model.groups |> Maybe.map Tuple.second |> Maybe.withDefault Dict.empty
            in
            ( { model
                | currentG =
                    if gt.treeType == Tree.Flat then
                        Nothing

                    else
                        Just uuid
                , groups = Dict.insert (Uuid.toString gt.uuid) ( gt, Dict.insert (Uuid.toString uuid) uuid gset ) model.groups
              }
            , Cmd.none
            )

        RemovedGroup gt uuid ->
            let
                gset =
                    Dict.get (Uuid.toString gt.uuid) model.groups |> Maybe.map Tuple.second |> Maybe.withDefault Dict.empty
            in
            ( { model
                | currentG = Nothing
                , groups =
                    Dict.insert
                        (Uuid.toString gt.uuid)
                        ( gt, Dict.remove (Uuid.toString uuid) gset )
                        model.groups
              }
            , Cmd.none
            )


inputGroups : Config -> Shared.Model -> Model -> Element Msg
inputGroups c s model =
    column [ spacing 10 ]
        [ h2 "Groups"
        , column [ alignTop, spacing 20, width <| minimum 200 fill ]
            [ text "In what kind of groups do you want to put this into?"
            , wrappedRow [ spacing 10 ]
                (s.state.groupTypes
                    |> Dict.values
                    |> List.map
                        (\gt ->
                            (if Just gt == model.currentGt then
                                button.primary

                             else
                                button.secondary
                            )
                                (Ok <| SetCurrentGT gt)
                                (displayZone s.state SmallcardTitle (Type.HType HType.GroupType) gt.uuid)
                        )
                )
            , column [ spacing 10 ]
                (model.groups
                    |> Dict.values
                    |> List.map
                        (\( gt, gdict ) ->
                            let
                                gtdisplay =
                                    displayZone s.state SmallcardTitle (Type.HType HType.GroupType) gt.uuid
                            in
                            column [ spacing 20 ]
                                [ wrappedRow [ width <| minimum 50 shrink, height (px 48), Border.width 2, padding 3, spacing 5, Border.color color.item.border ] <|
                                    (h2 <| gtdisplay ++ ":")
                                        :: (gdict
                                                |> Dict.values
                                                |> List.map
                                                    (\uuid ->
                                                        viewHalfCard s.state (Just (RemovedGroup gt uuid)) (Type.TType TType.Group) uuid
                                                    )
                                           )
                                ]
                        )
                )
            , model.currentGt
                |> Maybe.map
                    (\currentGt ->
                        column []
                            [ h2 <| "Select the groups this entity should belong to"
                            , wrappedRow [ padding 10, spacing 10, Border.color color.item.border ]
                                (s.state.groups
                                    |> Dict.filter
                                        (\_ g ->
                                            if currentGt.treeType == Tree.Flat then
                                                -- if the treeType of the currentGT is Node or Leaf,
                                                -- keep the groups that have the currentGroup as a parent
                                                Type.isChildOf s.state.types g.type_ currentGt.uuid
                                                    && containsScope
                                                        s.state.types
                                                        (c.mpuuid |> Maybe.map (HasUserType c.type_) |> Maybe.withDefault Empty)
                                                        g.scope

                                            else
                                                -- otherwise keep the groups whose type is a child of the current Group Type
                                                Type.isChildOf s.state.types currentGt.uuid g.type_
                                                    -- and those which are a direct child of the current Group
                                                    && (model.currentG |> Maybe.map (\currentG -> Tree.isDirectChildOf s.state.groups currentG g.uuid) |> Maybe.withDefault (g.parent == Nothing))
                                                    -- and those whose scope fits in the current Group
                                                    && containsScope s.state.types (c.mpuuid |> Maybe.map (HasUserType c.type_) |> Maybe.withDefault Empty) g.scope
                                        )
                                    |> Dict.values
                                    |> List.map .uuid
                                    |> List.map
                                        (\uuid ->
                                            tClickableCard s.state (AddedGroup currentGt uuid) (Type.TType TType.Group) uuid
                                        )
                                    |> withDefaultContent (p "(There are no Groups yet)")
                                )
                            ]
                    )
                |> Maybe.withDefault none
            ]
        ]
