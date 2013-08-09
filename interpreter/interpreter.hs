module Main where

import Data.Word (Word64)

data Id = Input | FoldBase | FoldStep

data P
  = Lambda E

data E
  = Zero
  | One
  | Id Id
  | If0 E E E
  | Fold E E E
  | Op1 Op1 E
  | Op2 Op2 E E

data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16
data Op2 = And | Or | Xor | Plus

-- | Size of programs.

sizeP :: P -> Int
sizeP (Lambda e) = 1 + sizeE e

-- | Size of expressions.

sizeE :: E -> Int
sizeE Zero = 1
sizeE One = 1
sizeE (Id _) = 1
sizeE (If0 e1 e2 e3) = 1 + sizeE e1 + sizeE e2 + sizeE e3
sizeE (Fold e1 e2 e3) = 2 + sizeE e1 + sizeE e2 + sizeE e3
sizeE (Op1 _ e1) = 1 + sizeE e1
sizeE (Op2 _ e1 e2) = 1 + sizeE e1 + sizeE e2

main = putStrLn "hello world"
