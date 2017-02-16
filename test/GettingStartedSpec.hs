module GettingStartedSpec (spec) where

import           Test.Hspec

import           Data.Json
import           Data.JsonParser

spec :: Spec
spec = do
 describe "encode" $ do
   it "json null" $ do
      (encode "null") `shouldBe` (Just jnull)
 describe "encode" $ do
   it "json boolean true" $ do
     (encode "true") `shouldBe` (Just $ jbool True)
 describe "encode" $ do
   it "json boolean false" $ do
     (encode "false") `shouldBe` (Just $ jbool False)
 describe "encode" $ do
   it "json integer 1" $ do
     (encode "1") `shouldBe` (Just $  jnumber 1.0)
 describe "encode" $ do
   it "json integer -1" $ do
     (encode "-1") `shouldBe` (Just $  jnumber (-1))
 describe "encode" $ do
   it "json integer 1.0" $ do
     (encode "1.0") `shouldBe` (Just $  jnumber 1.0)
 describe "encode" $ do
   it "json integer -1.00001" $ do
     (encode "1.00001") `shouldBe` (Just $  jnumber 1.00001)
 describe "encode" $ do
   it "json integer 1e3" $ do
     (encode "1e3") `shouldBe` (Just $  jnumber 1000)
 describe "encode" $ do
   it "json integer -1E-3" $ do
     (encode "-1E-3") `shouldBe` (Just $  jnumber (-0.001))
 describe "encode" $ do
   it "json integer -1.01e-3" $ do
     (encode "-1.01e-3") `shouldBe` (Just $  jnumber (-0.00101))
 describe "encode" $ do
   it "json string \"-fdskajfk\"" $ do
     (encode "\"-fdskajfk\"") `shouldBe` (Just $  jstring ("-fdskajfk"))
 describe "encode" $ do
   it "json string \"\\\"\"" $ do
     (encode "\"\\u34239\"") `shouldBe` (Just $  jstring "\34239")
 describe "encode" $ do
   it "json string \"\"" $ do
     (encode "\"\"") `shouldBe` (Just $  jstring "")
 describe "encode" $ do
   it "json string \"\"" $ do
     (encode "\"\\\b\\\n\\\f\\\r\\\t\\/\\\"\"") `shouldBe` (Just $  jstring "\b\n\f\r\t/\"")
