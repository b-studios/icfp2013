module Lahnparty.GeneratorTH where

import Lahnparty.Language
import Data.List(delete)
import Data.Word (Word64)

type Argument = Word64
type Result = Word64
type Size = Int
type InFold = Bool
type MustFold = Bool

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
--   Fold will only be used if the flag mustfold is set; but if the flag is set it will definitely be used.
--   (It must still be present in the  oplist).
findE :: Size -> [Op] -> InFold -> MustFold -> [E]
findE 1 _   _      True = []
findE 1 _   False  _    = [Id Input, One, Zero]
findE 1 _   True   _    = [Id Input, One, Zero, Id Byte, Id Acc]
findE 2 _   _      True = []
findE 2 ops infold _    = let ops1 = map (\(OpOp1 op) -> Op1 op) $ filter isOp1 ops 
                          in concat $ zipWith (map) ops1 (repeat (findE 1 undefined infold False))
findE n ops infold mustfold = if (n<5 && mustfold) 
                                then []
                                else concat $ map gen ops
  where
    ops' = delete OpFold ops

    gen :: Op -> [E]
    gen (OpOp1 op1) = map (Op1 op1) $ findE (n-1) ops infold mustfold
    gen (OpOp2 op2) = if mustfold
                        then [ (Op2 op2) e0 e1 |  i <- [5..((n-2) `div` 2)],                   -- optimization: e0 <=  e1, i starts at 5 to allow for fold
                                                 e0 <- findE i       ops  infold True,
                                                 e1 <- findE (n-1-i) ops' infold False]
                          ++ [ (Op2 op2) e0 e1 |  i <- [1..((n-2) `div` 2)],                   -- optimization: e0 <=  e1
                                                 e0 <- findE i       ops' infold False,
                                                 e1 <- findE (n-1-i) ops  infold True]

                        else [ (Op2 op2) e0 e1 |  i <- [1..((n-2) `div` 2)],                   -- optimization: e0 <=  e1
                                                 e0 <- findE i       ops infold False,
                                                 e1 <- findE (n-1-i) ops infold False]
    gen OpIf0       = if mustfold
                        then [ If0 e0 e1 e2 |  i <- [5..n-3], j <-[1..n-2-i], let k = n-1-i-j,
                                              e0 <- findE i ops  infold True,
                                              e1 <- findE j ops' infold False,
                                              e2 <- findE k ops' infold False]
                          ++ [ If0 e0 e1 e2 |  i <- [1..n-7], j <-[5..n-2-i], let k = n-1-i-j,
                                              e0 <- findE i ops' infold False,
                                              e1 <- findE j ops  infold True,
                                              e2 <- findE k ops' infold False]
                          ++ [ If0 e0 e1 e2 |  i <- [1..n-7], j <-[1..n-6-i], let k = n-1-i-j,
                                              e0 <- findE i ops' infold False,
                                              e1 <- findE j ops' infold False,
                                              e2 <- findE k ops  infold True]
                        else [ If0 e0 e1 e2 |  i <- [1..n-3], j <-[1..n-2-i], let k = n-1-i-j,
                                              e0 <- findE i ops infold False,
                                              e1 <- findE j ops infold False,
                                              e2 <- findE k ops infold False]
    gen OpFold      = if mustfold 
                        then [ Fold e0 e1 e2 |  i <- [1..n-4], j <-[1..n-3-i], let k = n-2-i-j,
                                                let newops = delete OpFold ops,
                                                e0 <- findE i newops False False,
                                                e1 <- findE j newops False False,
                                                e2 <- findE k newops True  False]
                        else []

