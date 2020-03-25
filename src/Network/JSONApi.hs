{- |
Entry-point module for this package.
-}
module Network.JSONApi
( D.Document
, D.ResourceData (..)
, D.ErrorDocument (..)
, D.Included
, E.Error (..)
, R.Relationship
, R.Resource (..)
, R.Relationships
, R.ResourcefulEntity (..)
, I.HasIdentifier (..)
, I.Identifier (..)
, L.Links
, M.Meta
, M.MetaObject (..)
, L.mkLinks
, P.Pagination (..)
, P.PageNum (..)
, P.PageSize (..)
, P.ResourceCount (..)
, R.indexLinks
, R.mkRelationship
, R.mkRelationships
, R.showLink
, D.mkDocument
, D.mkDocuments
, D.mkDocument'
, D.singleton
, D.list
, D.mkCompoundDocument
, D.mkCompoundDocuments
, D.mkCompoundDocument'
, D.mkIncludedResource
, D.mkSimpleDocument
, D.mkSimpleDocuments
, D.mkSimpleDocument'
, M.mkMeta
) where

import qualified Network.JSONApi.Error as E
import qualified Network.JSONApi.Document as D
import qualified Network.JSONApi.Identifier as I
import qualified Network.JSONApi.Link as L
import qualified Network.JSONApi.Meta as M
import qualified Network.JSONApi.Pagination as P
import qualified Network.JSONApi.Resource as R
