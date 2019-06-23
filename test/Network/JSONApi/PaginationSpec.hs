module Network.JSONApi.PaginationSpec where

import qualified Data.ByteString.Lazy.Char8 as BS

import           Network.JSONApi

import           Prelude hiding (id)

import           TestHelpers (prettyEncode)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "JSON serialization" $
    it "provides ToJSON instances" $ do
      let pagination = Pagination (PageSize 1) (PageNum 5) (ResourceCount 20)
      let encJson = BS.unpack . prettyEncode $ pagination
      encJson `shouldBe` "{\n    \"currentPage\": 5,\n    \"pageSize\": 1,\n    \"totalDocuments\": 20\n}"
