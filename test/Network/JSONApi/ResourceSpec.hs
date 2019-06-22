module Network.JSONApi.ResourceSpec where

import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Maybe (isJust, fromJust)
import           Data.Text (Text, pack)

import           GHC.Generics (Generic)

import           Network.JSONApi

import           TestHelpers (prettyEncode)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "JSON serialization" $
    it "can be encoded and decoded from JSON" $ do
      let encodedJson = BS.unpack . prettyEncode $ toResource testObject
      let decodedJson = AE.decode (BS.pack encodedJson) :: Maybe (Resource TestObject)
      isJust decodedJson `shouldBe` True

  describe "Self links" $
    it "can be constructed for a single high level resource" $ do
      let links = showLink testObject
      links `shouldBe` mkLinks [ ("self", "/TestObject/1") ]

  describe "Index links" $ do
    it "should always start at page 0" $ do
      let links = indexLinks testObject Nothing (PageSize 10) (PageNum 1) (ResourceCount 20)
      links `shouldBe` mkLinks [
          ("first","/TestObject?page%5Bnumber%5D=0&page%5Bsize%5D=10")
        , ("last","/TestObject?page%5Bnumber%5D=1&page%5Bsize%5D=10")
        , ("next","/TestObject?page%5Bnumber%5D=2&page%5Bsize%5D=10")
        , ("prev","/TestObject?page%5Bnumber%5D=0&page%5Bsize%5D=10")
        , ("self","/TestObject?page%5Bnumber%5D=1&page%5Bsize%5D=10")]

    it "should never have a negative prev page" $ do
      let links = indexLinks testObject Nothing (PageSize 10) (PageNum 0) (ResourceCount 20)
      links `shouldBe` mkLinks [
          ("first","/TestObject?page%5Bnumber%5D=0&page%5Bsize%5D=10")
        , ("last","/TestObject?page%5Bnumber%5D=1&page%5Bsize%5D=10")
        , ("next","/TestObject?page%5Bnumber%5D=1&page%5Bsize%5D=10")
        , ("prev","/TestObject?page%5Bnumber%5D=0&page%5Bsize%5D=10")
        , ("self","/TestObject?page%5Bnumber%5D=0&page%5Bsize%5D=10")]

    it "should handle a page size of 0" $ do
      let links = indexLinks testObject Nothing (PageSize 0) (PageNum 0) (ResourceCount 2)
      links `shouldBe` mkLinks [
          ("first","/TestObject?page%5Bnumber%5D=0&page%5Bsize%5D=1")
        , ("last","/TestObject?page%5Bnumber%5D=1&page%5Bsize%5D=1")
        , ("next","/TestObject?page%5Bnumber%5D=1&page%5Bsize%5D=1")
        , ("prev","/TestObject?page%5Bnumber%5D=0&page%5Bsize%5D=1")
        , ("self","/TestObject?page%5Bnumber%5D=0&page%5Bsize%5D=1")]

    it "should handle a negative page num" $ do
      let links = indexLinks testObject Nothing (PageSize 0) (PageNum $ -1) (ResourceCount 2)
      links `shouldBe` mkLinks [
          ("first","/TestObject?page%5Bnumber%5D=0&page%5Bsize%5D=1")
        , ("last","/TestObject?page%5Bnumber%5D=1&page%5Bsize%5D=1")
        , ("next","/TestObject?page%5Bnumber%5D=1&page%5Bsize%5D=1")
        , ("prev","/TestObject?page%5Bnumber%5D=0&page%5Bsize%5D=1")
        , ("self","/TestObject?page%5Bnumber%5D=0&page%5Bsize%5D=1")]

    it "should handle a negative resource count" $ do
      let links = indexLinks testObject Nothing (PageSize 1) (PageNum 0) (ResourceCount $ -1)
      links `shouldBe` mkLinks [
          ("first","/TestObject?page%5Bnumber%5D=0&page%5Bsize%5D=1")
        , ("last","/TestObject?page%5Bnumber%5D=-1&page%5Bsize%5D=1")
        , ("next","/TestObject?page%5Bnumber%5D=1&page%5Bsize%5D=1")
        , ("prev","/TestObject?page%5Bnumber%5D=0&page%5Bsize%5D=1")
        , ("self","/TestObject?page%5Bnumber%5D=0&page%5Bsize%5D=1")]

    it "should handle nested resource paths" $ do
      let links = indexLinks testObject (Just "/ParentObject/1") (PageSize 10) (PageNum 1) (ResourceCount 20)
      links `shouldBe` mkLinks [
          ("first","/ParentObject/1/TestObject?page%5Bnumber%5D=0&page%5Bsize%5D=10")
        , ("last","/ParentObject/1/TestObject?page%5Bnumber%5D=1&page%5Bsize%5D=10")
        , ("next","/ParentObject/1/TestObject?page%5Bnumber%5D=2&page%5Bsize%5D=10")
        , ("prev","/ParentObject/1/TestObject?page%5Bnumber%5D=0&page%5Bsize%5D=10")
        , ("self","/ParentObject/1/TestObject?page%5Bnumber%5D=1&page%5Bsize%5D=10")]

data TestObject = TestObject
  { myId :: Int
  , myName :: Text
  , myAge :: Int
  , myFavoriteFood :: Text
  } deriving (Show, Generic)

instance AE.ToJSON TestObject
instance AE.FromJSON TestObject

instance ResourcefulEntity TestObject where
  resourceIdentifier = pack . show . myId
  resourceType _ = "TestObject"
  resourceLinks _ = Just myResourceLinks
  resourceMetaData _ = Just myResourceMetaData
  resourceRelationships _ = Nothing

relationship :: Relationship
relationship =
  fromJust $ mkRelationship
    (Just $ Identifier "42" "FriendOfTestObject" Nothing)
    (Just myResourceLinks)

otherRelationship :: Relationship
otherRelationship =
  fromJust $ mkRelationship
    (Just $ Identifier "49" "CousinOfTestObject" Nothing)
    (Just myResourceLinks)

myResourceLinks :: Links
myResourceLinks =
  mkLinks [ ("self", "/me")
          , ("related", "/tacos/4")
          ]

myResourceMetaData :: Meta
myResourceMetaData = mkMeta (Pagination (Just 1) (Just 1) (Just 14))

testObject :: TestObject
testObject = TestObject 1 "Fred Armisen" 49 "Pizza"
