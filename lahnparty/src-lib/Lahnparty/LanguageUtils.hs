module Lahnparty.LanguageUtils where

import Lahnparty.Language

canonicalize :: E -> E
canonicalize (Op1 op Zero) | isShift op = Zero
canonicalize (Op2 _ Zero e) = canonicalize e

canonicalize (Op2 op1 (Op2 op2 e1 e2) e3) | op1 == op2 = Op2 op1 e1 (Op2 op1 e2 e3)

canonicalize e @ (Op2 op e1 e2) = if e1 <= e2 then e else Op2 op e2 e1

canonicalize (Op1 Not (Op1 Not e)) = canonicalize e
canonicalize (Fold e0 e1 Zero) = Zero
canonicalize (Fold e0 e1 One) = One
canonicalize (Fold e0 e1 (Id Input)) = Id Input
canonicalize (If0 Zero e1 e2) = canonicalize e1
canonicalize (If0 One e1 e2) = canonicalize e2
canonicalize e = e

isOp1 :: Op -> Bool
isOp1 (OpOp1 _) = True
isOp1 _ = False

isOp2 :: Op -> Bool
isOp2 (OpOp2 _) = True
isOp2 _ = False

isOp3 :: Op -> Bool
isOp3 OpIf0 = True
isOp3 OpFold = True
isOp3 _ = False

isShift :: Op1 -> Bool
isShift Not = False
isShift _ = True

arity :: Op -> Int
arity (OpOp1 _) = 1
arity (OpOp2 _) = 2
arity (OpIf0) = 3
arity (OpFold) = 3

weight :: Op -> Int
weight (OpFold) = 2
weight _ = 1
