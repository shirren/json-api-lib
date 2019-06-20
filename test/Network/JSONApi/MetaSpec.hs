module Network.JSONApi.MetaSpec where

import           Data.Aeson (ToJSON)
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Maybe (isJust)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Network.JSONApi
import           TestHelpers (prettyEncode)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "JSON serialization" $
    it "serializes/deserializes heterogeneous maps of ToJSON types" $ do
      let boolTestData = mkMeta testObject
      let encBoolJson = BS.unpack . prettyEncode $ boolTestData
      let decBoolJson = AE.decode (BS.pack encBoolJson) :: Maybe Meta
      isJust decBoolJson `shouldBe` True

testObject :: TestObject
testObject = TestObject 99 102 "Zapp Brannigan"

data TestObject = TestObject
  { myID :: Int
  , myAge :: Int
  , myName :: Text
  } deriving (Show, Generic)

instance ToJSON TestObject
instance MetaObject TestObject where
  typeName _ = "testObject"

data OtherTestObject = OtherTestObject
  { myFavoriteRestaurant :: Text
  , myDogsName :: Text
  , myDogsAge :: Int
  , myDogsFavoriteRestarant :: Text
  } deriving (Show, Generic)

instance ToJSON OtherTestObject
instance MetaObject OtherTestObject where
  typeName _ = "otherTestObject"
