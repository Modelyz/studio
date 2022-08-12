module Ident.Scope exposing (..)

import Dict exposing (Dict)
import Element exposing (..)
import Hierarchy.Hierarchic as Hierarchic exposing (Hierarchic)
import Hierarchy.Type as HType
import Item.Item as Item exposing (Item)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Typed.Type as TType


type
    Scope
    -- This is the scope of an identifier (TODO or a ZoneConfig)
    -- It allows to assign an identifier to allentities of type T
    -- TODO: AllEntitiesOfGroup or GroupScope Group (Maybe Group) ??
    -- or all the entities of a certain REA type:
    -- Scope could also be used in a group definition? TODO Move in its own module?
    -- all Typed items, possibly of type Uuid
    = TScope TType.Type (Maybe Uuid)
      -- all Hierarchic items, possibly of parent Uuid
    | HScope HType.Type (Maybe Uuid)


getParent : Dict String (Hierarchic (Item a)) -> Scope -> Maybe Scope
getParent all scope =
    -- get the parent scope but without the root typed scope
    case scope of
        TScope t mtuid ->
            Maybe.andThen (Item.find all) mtuid
                |> Maybe.andThen .parent
                |> Maybe.andThen (Item.find all)
                |> Maybe.map .parent
                |> Maybe.map (TScope t)

        HScope t mtuid ->
            Maybe.andThen (Item.find all) mtuid
                |> Maybe.andThen .parent
                |> Maybe.andThen (Item.find all)
                |> Maybe.map .parent
                |> Maybe.map (HScope t)


getParentsToRoot : Item a -> Scope -> Dict String (Hierarchic (Item a)) -> List Scope -> List Scope
getParentsToRoot initial scope all currentList =
    case scope of
        TScope t m ->
            getParent all scope
                |> Maybe.map (\parentScope -> getParentsToRoot initial parentScope all currentList)
                |> Maybe.withDefault (TScope t Nothing :: scope :: currentList)

        HScope t m ->
            getParent all scope
                |> Maybe.map (\parentScope -> getParentsToRoot initial parentScope all currentList)
                |> Maybe.withDefault (HScope t Nothing :: scope :: currentList)


isParentOf : Scope -> Dict String (Hierarchic (Item a)) -> Scope -> Bool
isParentOf childScope all parentScope =
    -- TODO review !
    case parentScope of
        TScope parentType parentTypeMaybeUuid ->
            False

        HScope parentType parentTypeMaybeUuid ->
            case parentTypeMaybeUuid of
                Nothing ->
                    case childScope of
                        HScope childType _ ->
                            parentType == childType

                        TScope _ _ ->
                            False

                Just parentTypeUuid ->
                    case childScope of
                        HScope childType childTypeMaybeUuid ->
                            case childTypeMaybeUuid of
                                Nothing ->
                                    False

                                Just childTypeUuid ->
                                    (parentType == childType)
                                        && (Maybe.map3 Hierarchic.isAscendantOf (Item.find all childTypeUuid) (Just all) (Item.find all parentTypeUuid)
                                                |> Maybe.withDefault False
                                           )

                        TScope _ _ ->
                            False


toString : Scope -> String
toString id =
    case id of
        TScope _ _ ->
            "TScope"

        HScope _ _ ->
            "HScope"


encode : Scope -> Encode.Value
encode e =
    case e of
        TScope type_ mtuid ->
            Encode.object <|
                [ ( "for", Encode.string "TScope" )
                , ( "type", TType.encode type_ )
                ]
                    ++ (Maybe.map (\uuid -> [ ( "uuid", Uuid.encode uuid ) ]) mtuid |> Maybe.withDefault [])

        HScope type_ mtuid ->
            Encode.object <|
                [ ( "for", Encode.string "HScope" )
                , ( "type", HType.encode type_ )
                ]
                    ++ (Maybe.map (\uuid -> [ ( "uuid", Uuid.encode uuid ) ]) mtuid |> Maybe.withDefault [])


decoder : Decoder Scope
decoder =
    Decode.field "for" Decode.string
        |> Decode.andThen
            (\for ->
                case for of
                    "TScope" ->
                        Decode.map2 TScope
                            (Decode.field "type" TType.decoder)
                            (Decode.maybe (Decode.field "uuid" Uuid.decoder))

                    "HScope" ->
                        Decode.map2 HScope
                            (Decode.field "type" HType.decoder)
                            (Decode.maybe (Decode.field "uuid" Uuid.decoder))

                    _ ->
                        Decode.fail "Cannot decode the scope of this identifier type"
            )


compare : Scope -> String
compare s =
    toString s
        ++ " "
        ++ (case s of
                TScope type_ mtuid ->
                    TType.toString type_ ++ (Maybe.map (\uuid -> " " ++ Uuid.toString uuid) mtuid |> Maybe.withDefault "")

                HScope type_ mtuid ->
                    HType.toString type_ ++ (Maybe.map (\uuid -> " " ++ Uuid.toString uuid) mtuid |> Maybe.withDefault "")
           )
