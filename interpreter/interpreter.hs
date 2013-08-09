module Main where

import Data.Bits
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

-- | Evaluate programs.

evalP :: Word64 -> P -> Word64
evalP input (Lambda e) =
  evalE input (error "not in fold") (error "not in fold") e

-- | Evaluate expressions.

evalE :: Word64 -> Word64 -> Word64 -> E -> Word64
evalE input foldBase foldStep Zero = 0
evalE input foldBase foldStep One = 1
evalE input foldBase foldStep (Id Input) = input
evalE input foldBase foldStep (Id FoldBase) = foldBase
evalE input foldBase foldStep (Id FoldStep) = foldStep
evalE input foldBase foldStep (If0 e1 e2 e3) =
  if evalE input foldBase foldStep e1 == 0
    then evalE input foldBase foldStep e2
    else evalE input foldBase foldStep e3
evalE input foldBase foldStep (Fold e0 e1 e2) = foldr f initial values
  where
    values = listOfFoldedValues (evalE input foldBase foldStep e0)
    initial = evalE input foldBase foldStep e1
    f x y = evalE input x y e2
evalE input foldBase foldStep (Op1 op1 e1) =
  evalOp1 op1 (evalE input foldBase foldStep e1)
evalE input foldBase foldStep (Op2 op2 e1 e2) =
  evalOp2 op2 (evalE input foldBase foldStep e1) (evalE input foldBase foldStep e2)

evalOp1 :: Op1 -> Word64 -> Word64
evalOp1 Not = complement
evalOp1 Shl1 = (`shiftL` 1)
evalOp1 Shr1 = (`shiftR` 1)
evalOp1 Shr4 = (`shiftR` 4)
evalOp1 Shr16 = (`shiftR` 16)

evalOp2 :: Op2 -> Word64 -> Word64 -> Word64
evalOp2 And = (.&.)
evalOp2 Or = (.|.)
evalOp2 Xor = xor
evalOp2 Plus = (+)

listOfFoldedValues :: Word64 -> [Word64]
listOfFoldedValues =
  take 8 . map (`shiftR` 56) . iterate (`shiftL` 8)

main = putStrLn "hello world"
