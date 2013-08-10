module Lahnparty.Language where

import Data.Bits
import Data.Word (Word64)

data Id =
  -- | The overall input (bound by the top-level lambda).
  Input

  -- | The current byte (first variable bound by lambda in fold).
  | Byte

  -- | The current accumulator (second variable bound by lambda in fold).
  | Acc
    deriving Eq

data P
  = Lambda !E
    deriving Eq

data E
  = Zero
  | One
  | Id !Id
  | If0 !E !E !E
  | Fold !E !E !E
  | Op1 !Op1 !E
  | Op2 !Op2 !E !E
    deriving Eq

data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16
    deriving (Eq, Ord)

data Op2 = And | Or | Xor | Plus
    deriving (Eq, Ord)

data Op
  = OpOp1 !Op1
  | OpOp2 !Op2
  | OpIf0
  | OpFold 
  | OpTFold
    deriving (Eq, Ord)

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
evalE input byte acc Zero = 0
evalE input byte acc One = 1
evalE input byte acc (Id Input) = input
evalE input byte acc (Id Byte) = byte
evalE input byte acc (Id Acc) = acc
evalE input byte acc (If0 e1 e2 e3) =
  if evalE input byte acc e1 == 0
    then evalE input byte acc e2
    else evalE input byte acc e3
evalE input byte acc (Fold e0 e1 e2) = foldr f initial values
  where
    values = listOfFoldedValues (evalE input byte acc e0)
    initial = evalE input byte acc e1
    f x y = evalE input x y e2
evalE input byte acc (Op1 op1 e1) =
  evalOp1 op1 (evalE input byte acc e1)
evalE input byte acc (Op2 op2 e1 e2) =
  evalOp2 op2 (evalE input byte acc e1) (evalE input byte acc e2)

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

--
-- * Pretty printer
--

-- | Pretty print programs.
prettyP :: P -> String
prettyP (Lambda e) = "(lambda (" ++ prettyId Input ++ ") " ++ prettyE e ++ ")"

-- | Pretty print expressions.
prettyE :: E -> String
prettyE Zero            = "0"
prettyE One             = "1"
prettyE (Id x)          = prettyId x
prettyE (If0  e1 e2 e3) = "(if0 "  ++ prettyE e1 ++ " " 
                                   ++ prettyE e2 ++ " "
                                   ++ prettyE e3 ++ ")"
prettyE (Fold e1 e2 e3) = "(fold " ++ prettyE e1 ++ " "
                                   ++ prettyE e2 ++ " (lambda ("
                                   ++ prettyId Byte ++ " "
                                   ++ prettyId Acc ++ ") "
                                   ++ prettyE e3 ++ "))"
prettyE (Op1 o e)       = "(" ++ prettyOp1 o ++ " "
                              ++ prettyE e ++ ")"
prettyE (Op2 o e1 e2)   = "(" ++ prettyOp2 o ++ " "
                              ++ prettyE e1 ++ " "
                              ++ prettyE e2 ++ ")"

-- | Register names.
prettyId Input = "input"
prettyId Byte  = "byte"
prettyId Acc   = "acc"

-- | Op1 keywords.
prettyOp1 :: Op1 -> String
prettyOp1 Not   = "not"
prettyOp1 Shl1  = "shl1"
prettyOp1 Shr1  = "shr1"
prettyOp1 Shr4  = "shr4"
prettyOp1 Shr16 = "shr16"

-- | Op2 keywords.
prettyOp2 :: Op2 -> String
prettyOp2 And  = "and"
prettyOp2 Or   = "or"
prettyOp2 Xor  = "xor"
prettyOp2 Plus = "plus"

instance Show Id  where show = prettyId
instance Show P   where show = prettyP
instance Show E   where show = prettyE
instance Show Op1 where show = prettyOp1
instance Show Op2 where show = prettyOp2

instance Show Op where
  show (OpOp1 o) = show o
  show (OpOp2 o) = show o
  show OpIf0     = "if0"
  show OpFold    = "fold"
  show OpTFold   = "tfold"
