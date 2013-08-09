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

main = putStrLn "hello world"
