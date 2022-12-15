module Group.Input exposing (Config, Model, Msg, added, init, inputGroups, removed, update)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import GroupType.GroupType as GroupType exposing (GroupType)
import Hierarchy.Type as HType
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Shared
import Type exposing (Type)
import Typed.Type as TType
import Util exposing (third)
import View exposing (..)
import View.Smallcard exposing (tClickableCard, viewHalfCard)
import View.Style exposing (..)
import Zone.View exposing (display)
import Zone.Zone exposing (Zone(..))


type alias Config =
    { type_ : Type
    , mpuuid : Maybe Uuid
    }


type alias Model =
    { oldGroups : Dict String Uuid

    -- gt uuid as keys, (gt, group uuids) as values
    , currentGt : Maybe GroupType
    , groups : Dict String ( GroupType, Dict String Uuid )
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
            ( { model | groups = Dict.insert (Uuid.toString gt.uuid) ( gt, Dict.insert (Uuid.toString uuid) uuid gset ) model.groups }, Cmd.none )

        RemovedGroup gt uuid ->
            let
                gset =
                    Dict.get (Uuid.toString gt.uuid) model.groups |> Maybe.map Tuple.second |> Maybe.withDefault Dict.empty
            in
            ( { model
                | groups =
                    Dict.insert
                        (Uuid.toString gt.uuid)
                        ( gt, Dict.remove (Uuid.toString uuid) gset )
                        model.groups
              }
            , Cmd.none
            )


inputGroups : Config -> Shared.Model -> Model -> Element Msg
inputGroups c s model =
    column [ alignTop, spacing 20, width <| minimum 200 fill ]
        [ text "In what kind of group do you want to put this into?"
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
                            (SetCurrentGT gt)
                            (display s.state.types s.state.configs SmallcardTitle s.state.identifiers s.state.grouped (Type.HType HType.GroupType) gt.uuid)
                    )
            )
        , column [ spacing 10 ]
            (model.groups
                |> Dict.values
                |> List.map
                    (\( gt, gdict ) ->
                        let
                            gtdisplay =
                                display s.state.types s.state.configs SmallcardTitle s.state.identifiers s.state.grouped (Type.HType HType.GroupType) gt.uuid
                        in
                        column [ spacing 20 ]
                            [ wrappedRow [ width <| minimum 50 shrink, height (px 48), Border.width 2, padding 3, spacing 5, Border.color color.item.border ] <|
                                (h2 <| gtdisplay ++ ":")
                                    :: (gdict
                                            |> Dict.values
                                            |> List.map
                                                (\uuid ->
                                                    viewHalfCard (Just (RemovedGroup gt uuid)) s.state.types s.state.configs s.state.identifiers s.state.grouped (Type.TType TType.Group) uuid
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
                                        Type.isChildOf s.state.types g.type_ currentGt.uuid
                                            && containsScope
                                                s.state.types
                                                (c.mpuuid |> Maybe.map (HasUserType c.type_) |> Maybe.withDefault Empty)
                                                g.scope
                                    )
                                |> Dict.values
                                |> List.map .uuid
                                |> List.map
                                    (\uuid ->
                                        tClickableCard (AddedGroup currentGt uuid) s.state.types s.state.configs s.state.identifiers s.state.grouped (Type.TType TType.Group) uuid
                                    )
                                |> withDefaultContent (p "(There are no Groups yet)")
                            )
                        ]
                )
            |> Maybe.withDefault none
        ]
