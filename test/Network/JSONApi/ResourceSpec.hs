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
spec =
  describe "JSON serialization" $
    it "can be encoded and decoded from JSON" $ do
      let encodedJson = BS.unpack . prettyEncode $ toResource testObject
      let decodedJson = AE.decode (BS.pack encodedJson) :: Maybe (Resource TestObject)
      isJust decodedJson `shouldBe` True

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
