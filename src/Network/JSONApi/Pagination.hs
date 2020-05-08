module Network.JSONApi.Pagination (
    Pagination (..)
  , PageNum (..)
  , PageSize (..)
  , ResourceCount (..)
) where

import Control.DeepSeq (NFData)

import Data.Aeson ((.=), ToJSON, object, toJSON)

import qualified GHC.Generics as G

import Network.JSONApi.Meta (MetaObject (..))

{- |
Wrapper type for the various components of pagination being page size, page number
and the number of resources in total.
-}
data Pagination = Pagination {
    getPaginationPageSize :: PageSize
  , getPaginationPageNum :: PageNum
  , getPaginationResourceCount :: ResourceCount
} deriving (Eq, G.Generic)

instance ToJSON Pagination where
  toJSON (Pagination (PageSize size) (PageNum num) (ResourceCount count)) =
    object [
        "pageSize" .= size
      , "currentPage" .= num
      , "totalDocuments" .= count
      ]

instance NFData Pagination

{- |
Pagination can be used as a meta object if required in addition to the links generated
for paging.
-}
instance MetaObject Pagination where
  typeName _ = "pagination"

{- |
We can specify limits on the number of rows we would like back from the database
-}
newtype PageSize = PageSize {
  getPageSize :: Int
} deriving (Eq, G.Generic, Show)

instance NFData PageSize

newtype PageNum = PageNum {
  getPageNum :: Int
} deriving (Eq, G.Generic, Show)

instance NFData PageNum

newtype ResourceCount = ResourceCount {
  getResourceCount :: Int
} deriving (Eq, G.Generic, Show)

instance NFData ResourceCount
