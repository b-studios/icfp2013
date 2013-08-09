module Lahnparty.GeneratorTH where

import Lahnparty.Language
import Data.List(delete)
import Data.Word (Word64)

type Argument = Word64
type Result = Word64
type Size = Int
type InFold = Bool

data Op = OpOp1 Op1
        | OpOp2 Op2
        | OpIf0
        | OpFold deriving (Show, Eq, Ord)

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

arity :: Op -> Int
arity (OpOp1 _) = 1
arity (OpOp2 _) = 2
arity (OpIf0) = 3
arity (OpFold) = 3

weight :: Op -> Int
weight (OpFold) = 2
weight _ = 1


generate :: Int -> [Op] -> [(Argument, Result)] -> [P]
generate size ops points = undefined


-- | Generates expressions of given size using (a subset) of given operators. 
--   May omit expressions that have shorter equivalents (but currently does not).
findE :: Size -> [Op] -> InFold -> [E]
findE 1 _  False = [Id Input, One, Zero]
findE 1 _  True  = [Id Input, One, Zero, Id Byte, Id Acc]
findE 2 ops infold = let ops1 = map (\(OpOp1 op) -> Op1 op) $ filter isOp1 ops 
                     in concat $ zipWith (map) ops1 (repeat (findE 1 undefined infold))
findE n ops infold = concat $ map gen ops
  where
    gen :: Op -> [E]
    gen (OpOp1 op1) = map (Op1 op1) $ findE (n-1) ops infold
    gen (OpOp2 op2) = [ (Op2 op2) e0 e1 |  i <- [1..n-2],
                                          e0 <- findE i       ops infold,
                                          e1 <- findE (n-1-i) ops infold]
    gen OpIf0       = [ If0 e0 e1 e2 |  i <- [1..n-3], j <-[1..n-2-i], let k = n-1-i-j,
                                       e0 <- findE i ops infold,
                                       e1 <- findE j ops infold,
                                       e2 <- findE k ops infold]
    gen OpFold      = [ Fold e0 e1 e2 |  i <- [1..n-4], j <-[1..n-3-i], let k = n-2-i-j,
                                        e0 <- findE i (delete OpFold ops) True,
                                        e1 <- findE j (delete OpFold ops) True,
                                        e2 <- findE k (delete OpFold ops) True]

