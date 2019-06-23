module Network.JSONApi.Pagination (
    Pagination (..)
  , PageNum (..)
  , PageSize (..)
  , ResourceCount (..)
) where

import Data.Aeson ((.=), ToJSON, object, toJSON)

import Network.JSONApi.Meta (MetaObject (..))

{- |
Wrapper type for the various components of pagination being page size, page number
and the number of resources in total.
-}
data Pagination = Pagination {
    getPaginationPageSize :: PageSize
  , getPaginationPageNum :: PageNum
  , getPaginationResourceCount :: ResourceCount
}

instance ToJSON Pagination where
  toJSON (Pagination (PageSize size) (PageNum num) (ResourceCount count)) =
    object [
        "pageSize" .= size
      , "currentPage" .= num
      , "totalDocuments" .= count
      ]

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
} deriving Show

newtype PageNum = PageNum {
  getPageNum :: Int
} deriving Show

newtype ResourceCount = ResourceCount {
  getResourceCount :: Int
} deriving Show
