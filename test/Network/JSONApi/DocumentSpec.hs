module Network.JSONApi.DocumentSpec where

import Control.Lens ((^?))

import           Data.Aeson (ToJSON)
import qualified Data.Aeson as AE
import qualified Data.Aeson.Lens as Lens
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Either (isRight)
import           Data.Maybe

import           Network.JSONApi

import           TestHelpers
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "JSON serialization" $ do
    it "JSON encodes/decodes a singleton resource" $ do
      let jsonApiObj = mkDocument testObject Nothing Nothing
          encodedJson = encodeDocumentObject jsonApiObj
          decodedJson = decodeDocumentObject encodedJson

      isRight decodedJson `shouldBe` True

    it "JSON encodes/decodes a list of resources" $ do
      let jsonApiObj = mkDocuments [testObject, testObject2] Nothing Nothing
          encodedJson = encodeDocumentObject jsonApiObj
          decodedJson = decodeDocumentObject encodedJson

      isRight decodedJson `shouldBe` True

    it "contains the allowable top-level keys" $ do
      let jsonApiObj = mkDocument testObject Nothing Nothing
          encodedJson = encodeDocumentObject jsonApiObj
          dataObject = encodedJson ^? Lens.key "data"
          linksObject = encodedJson ^? Lens.key "links"
          metaObject = encodedJson ^? Lens.key "meta"
          includedObject = encodedJson ^? Lens.key "included"

      isJust dataObject `shouldBe` True
      isJust linksObject `shouldBe` True
      isJust metaObject `shouldBe` True
      isJust includedObject `shouldBe` True

    it "allows an optional top-level links object" $ do
      let jsonApiObj = mkDocument testObject (Just linksObj) Nothing
          encodedJson = encodeDocumentObject jsonApiObj
          decodedJson = decodeDocumentObject encodedJson

      isRight decodedJson `shouldBe` True

    it "allows an optional top-level meta object" $ do
      let jsonApiObj = mkDocument testObject Nothing (Just testMetaObj)
          encodedJson = encodeDocumentObject jsonApiObj
          decodedJson = decodeDocumentObject encodedJson

      isRight decodedJson `shouldBe` True

    it "allows a heterogeneous list of related resources" $ do
      let includedResources = mkIncludedResource testObject
          jsonApiObj = mkCompoundDocument testObject Nothing Nothing includedResources
          encodedJson = encodeDocumentObject jsonApiObj
          decodedJson = decodeDocumentObject encodedJson

      isRight decodedJson `shouldBe` True

decodeDocumentObject :: ByteString -> Either String (Document TestResource)
decodeDocumentObject = AE.eitherDecode

encodeDocumentObject :: (ToJSON a) => Document a -> ByteString
encodeDocumentObject = prettyEncode
