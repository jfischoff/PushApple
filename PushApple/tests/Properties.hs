{-# LANGUAGE OverloadedStrings #-}
module ApplePush2.Properties where

import ApplePush2.Types
import ApplePush2.Instances
import qualified Test.QuickCheck.Checkers as C
import Test.QuickCheck
import qualified Data.Aeson as A
import qualified Data.Attoparsec.Lazy as L
import qualified Data.Attoparsec.Number as N
import qualified Data.Binary as B
import Data.Aeson.Parser (value)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (Test, defaultMain, testGroup)
import qualified Data.HashMap.Strict as H
import ApplePush2.Classes
import Data.Maybe

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
            testGroup "json_round_trip" [
                testProperty "alert" $ roundTripEq (Simple ""),
                testProperty "notification" $ roundTripEq emptyNotification
            ],
            testGroup "binary_round_trip" [
                testProperty "value" $ value_bytestring_round_trip,
                testProperty "notification" $ binaryRoundTripEq emptyNotification,
                testProperty "payload" $ binaryRoundTripEq emptyPayload
            ]
        ]
        
value_bytestring_round_trip :: A.Value -> Bool
value_bytestring_round_trip x = (fromJust $ A.decodeWith value A.fromJSON $ A.encode x) =~= x

binaryRoundTripEq :: (B.Binary a, Equivalent a) => a -> a -> Bool
binaryRoundTripEq _ x = x =~= (B.decode . B.encode) x

encodeRoundTrip :: (A.FromJSON a, A.ToJSON a, Equivalent a) => a -> a -> Bool
encodeRoundTrip _ x = x =~= (fromJust $ A.decode $ A.encode x)
    
roundTrip :: (A.FromJSON a, A.ToJSON a) => (a -> a -> Bool) -> a -> a -> Bool
roundTrip eq _ i =
    case A.fromJSON . A.toJSON $ i of
      A.Success v -> v `eq` i
      _           -> False

roundTripEq :: (Equivalent a, A.FromJSON a, A.ToJSON a) => a -> a -> Bool
roundTripEq x y = roundTrip (=~=) x y


