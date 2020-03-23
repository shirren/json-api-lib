{- |
Module representing a JSON-API resource object.

Specification: <http://jsonapi.org/format/#document-resource-objects>
-}
module Network.JSONApi.Resource
( Resource (..)
, Relationships
, ResourcefulEntity (..)
, Relationship
, indexLinks
, mkRelationship
, mkRelationships
, showLink
) where

import           Control.DeepSeq (NFData)

import           Control.Lens.TH

import           Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?))
import qualified Data.Aeson as AE
import           Data.Aeson.Types (fieldLabelModifier)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text, pack)

import           GHC.Generics hiding (Meta)

import           Network.JSONApi.Identifier (HasIdentifier (..), Identifier (..))
import           Network.JSONApi.Link (Links, mkLinks)
import           Network.JSONApi.Meta (Meta)
import           Network.JSONApi.Pagination (Pagination (..), PageSize (..), PageNum (..), ResourceCount (..))
import           Network.URI.Encode (encodeText)

import           Prelude hiding (id)

{- |
Type representing a JSON-API resource object.

A Resource supplies standardized data and metadata about a resource.

Specification: <http://jsonapi.org/format/#document-resource-objects>
-}
data Resource a = Resource
  { getIdentifier :: Identifier
  , getResource :: a
  , getLinks :: Maybe Links
  , getRelationships :: Maybe Relationships
  } deriving (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (Resource a) where
  toJSON (Resource (Identifier resId resType metaObj) resObj linksObj rels) =
    AE.object [ "id"            .= resId
              , "type"          .= resType
              , "attributes"    .= resObj
              , "links"         .= linksObj
              , "meta"          .= metaObj
              , "relationships" .= rels
              ]

instance (FromJSON a) => FromJSON (Resource a) where
  parseJSON = AE.withObject "resourceObject" $ \v -> do
    id    <- v .: "id"
    typ   <- v .: "type"
    attrs <- v .: "attributes"
    links <- v .:? "links"
    meta  <- v .:? "meta"
    rels  <- v .:? "relationships"
    return $ Resource (Identifier id typ meta) attrs links rels

instance HasIdentifier (Resource a) where
  identifier = getIdentifier

instance (NFData a) => NFData (Resource a)

{- |
A typeclass for decorating an entity with JSON API properties
-}
class (ToJSON a, FromJSON a) => ResourcefulEntity a where
  resourceIdentifier :: a -> Text
  resourceType :: a -> Text
  resourceLinks :: a -> Maybe Links
  resourceMetaData :: a -> Maybe Meta
  resourceRelationships :: a -> Maybe Relationships

  fromResource :: Resource a -> a
  fromResource = getResource

  toResource :: a -> Resource a
  toResource a =
    Resource
      (Identifier (resourceIdentifier a) (resourceType a) (resourceMetaData a))
      a
      (resourceLinks a)
      (resourceRelationships a)

{- |
A type representing the Relationship between 2 entities

A Relationship provides basic information for fetching further information
about a related resource.

Specification: <http://jsonapi.org/format/#document-resource-object-relationships>
-}
data Relationship = Relationship
  { _data :: Maybe Identifier
  , _links :: Maybe Links
  } deriving (Show, Eq, Generic)

instance ToJSON Relationship where
  toJSON = AE.genericToJSON
    AE.defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Relationship where
  parseJSON = AE.genericParseJSON
    AE.defaultOptions { fieldLabelModifier = drop 1 }

instance NFData Relationship

newtype Relationships = Relationships (Map Text Relationship)
  deriving (Show, Eq, Generic)

instance ToJSON Relationships
instance FromJSON Relationships
instance NFData Relationships

mkRelationships :: Relationship -> Relationships
mkRelationships rel =
  Relationships $ Map.singleton (relationshipType rel) rel

relationshipType :: Relationship -> Text
relationshipType relationship = case _data relationship of
  Nothing -> "unidentified"
  (Just (Identifier _ typ _)) -> typ

{- |
Constructor function for creating a Relationship record

A relationship must contain either an Identifier or a Links record
-}
mkRelationship :: Maybe Identifier -> Maybe Links -> Maybe Relationship
mkRelationship Nothing Nothing = Nothing
mkRelationship resId links = Just $ Relationship resId links

makeLenses ''Resource

{- |
Helper function to build relative links for a single resource of type ResourceEntity
-}
showLink :: ResourcefulEntity e => e -> Links
showLink resource = mkLinks [ ("self", buildLink) ]
  where
    buildLink = "/" <> resourceType resource <> "/" <> resourceIdentifier resource

{- |
Helper function to beuild relative links for a collection of resources of type ResourceEntity.

This helper function assumes that the first page is always page 0.
-}
indexLinks :: Text -> Pagination -> Links
indexLinks baseUrl (Pagination pageSize pageNum resourceCount) = mkLinks [
     ("self", genLink pgNum)
    ,("first", genLink (0 :: Int))
    ,("prev", genLink (if pgNum - 1 < 0 then 0 else pgNum - 1))
    ,("next", genLink (pgNum + 1))
    ,("last", genLink ((resCount `quot` pgSize) - 1))]
  where
    pgNum = if getPageNum pageNum < 0 then 0 else getPageNum pageNum
    pgSize = if getPageSize pageSize <= 0 then 1 else getPageSize pageSize
    resCount = if getResourceCount resourceCount < 0 then 0 else getResourceCount resourceCount
    genLink no =  baseUrl <> "?" <> encodeText "page[number]" <> "=" <> (pack . show) no <>
                  "&" <> encodeText "page[size]" <> "=" <> (pack . show) pgSize
