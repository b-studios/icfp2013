{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NoMonomorphismRestriction #-}
module Lahnparty.Generator1 where

import Lahnparty.Language
import Test.QuickCheck
import Control.Applicative

import qualified Data.Set as S
import Data.Set (Set)
import Control.Monad.State

-- import System.Random

-- Untested,
-- Minimal QuickCheck generator


data Operator = Oper1 (Set Op1)
              | Oper2 (Set Op2)
              | OperIf0
              | OperFold
              | OperTFold
                deriving (Eq, Ord)

data Params = Params { ops :: Set Operator
                     , inFold :: Bool
                     }

instance Arbitrary Id where
  arbitrary = elements [Input, Byte, Acc]

instance Arbitrary (State Params Id) where
  arbitrary = promote $
            elements <$> ((\x -> [Input] ++ if (inFold x) then [Byte, Acc] else []) <$> get)

instance Arbitrary Op1 where
  arbitrary = elements [Not, Shl1, Shr1, Shr4, Shr16]

instance Arbitrary Op2 where
  arbitrary = elements [And, Or, Xor, Plus]

constantE = elements [Zero, One]
arbitraryVariable = Id <$> arbitrary

arbitraryFoldParam arbOp = Fold <$> arbitrary <*> arbitrary <*> arbOp
arbitraryFold =
  (\arbOp -> [ arbitraryFoldParam arbOp ]) <$>
                   do
                     modify (\p -> p {inFold = True})
                     return arbitrary

arbitraryFirstOrderProgGens =
  [ constantE
  , arbitraryVariable
  , If0 <$> arbitrary <*> arbitrary <*> arbitrary
  , Op1 <$> arbitrary <*> arbitrary
  , Op2 <$> arbitrary <*> arbitrary <*> arbitrary
  ]

instance Arbitrary E where
  arbitrary =
    oneof
    (arbitraryFirstOrderProgGens ++ [ Fold <$> arbitrary <*> arbitrary <*> arbitrary ])


instance Arbitrary (State Params E) where
  arbitrary =
    promote $ oneof <$> ((arbitraryFirstOrderProgGens ++) <$> arbitraryFold)

