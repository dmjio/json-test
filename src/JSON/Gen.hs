--------------------------------------------------------------------------------
-- |
-- Module      : JSON.Gen
-- Description : Mock JSON generator
-- Maintainer  : David Johnson <david@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module JSON.Gen where
--------------------------------------------------------------------------------
import Data.Aeson
import Test.QuickCheck
import Test.QuickCheck.Instances
import JSON.Parser
import Data.Vector
--------------------------------------------------------------------------------
-- | Round trip tests of json parsing
roundTrip :: IO ()
roundTrip =
  quickCheckWith stdArgs { maxSize = 10, maxSuccess = 10 } $
    forAll genValue $ \value ->
       parse (encode value) === eitherDecode (encode value)
--------------------------------------------------------------------------------
-- | Arbitrary on 'Value'
instance Arbitrary Value where
  arbitrary = genValue
--------------------------------------------------------------------------------
-- | 'Gen' 'Value' for creating fake JSON data
genValue :: Gen Value
genValue =
  oneof [ String <$> arbitrary
        , Number <$> arbitrary
        , pure Null
        , Bool <$> arbitrary
        , Object <$> arbitrary
        , Array <$> arbitrary
        ]
