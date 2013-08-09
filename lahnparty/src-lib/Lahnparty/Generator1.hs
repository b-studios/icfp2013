module Lahnparty.Generator1 where

import Lahnparty.Language
import Test.QuickCheck
import Control.Applicative

import System.Random

-- Untested,
-- Minimal QuickCheck generator

instance Arbitrary Id where
  arbitrary = elements [Input, Byte, Acc]

instance Arbitrary Op1 where
  arbitrary = elements [Not, Shl1, Shr1, Shr4, Shr16]

instance Arbitrary Op2 where
  arbitrary = elements [And, Or, Xor, Plus]

constantE = elements [Zero, One]
arbitraryVariable = Id <$> arbitrary

instance Arbitrary E where
  arbitrary = oneof
              [ constantE
              , arbitraryVariable
              , If0 <$> arbitrary <*> arbitrary <*> arbitrary
              , Op1 <$> arbitrary <*> arbitrary
              , Op2 <$> arbitrary <*> arbitrary <*> arbitrary
              , Fold <$> arbitrary <*> arbitrary <*> arbitrary
              ]
