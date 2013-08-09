module Main where

-- only three variables per program anyway?!
--
-- data Id = Input | FoldBase | FoldStep

type Id = String

data P
  = PLambda Id E

data E
  = Zero
  | One
  | Id
  | If0 E E E
  | Fold E E Id Id E
  | Op1 Op1 E
  | Op2 Op2 E E

data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16
data Op2 = And | Or | Xor | Plus

main = putStrLn "hello world"
