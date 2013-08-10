module Lahnparty.GeneratorTH where

import Lahnparty.Language
import Lahnparty.Types
import Data.List(delete)
import Data.Word (Word64)
import Debug.Trace

type Argument = Word64
type Result = Word64
type InFold = Bool
type MustFold = Bool

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


type Generator = Size -> [Op] -> [P]

findP :: Generator
findP size ops =
  if OpTFold `elem` ops
   then
     -- assertFalse (OpFold `elem` ops)

     -- XXX This findE should produce a fold at the top level
     -- map Lambda $ findETopFold (size - 1) (delete OpTFold ops)
     map Lambda $ concatMap (\s -> findETopFold s (delete OpTFold ops)) [5..size - 1]
   else
     --map Lambda $ findE (size - 1) ops False (OpFold `elem` ops)
     map Lambda $ concatMap (\s -> findE s ops False (OpFold `elem` ops)) [1..size - 1]



newtype SizedE = SizedE E
            deriving Eq
instance Ord SizedE where
  -- XXX Is this a valid order? I think so, since it's just a special lexicographic ordering. PG
  compare (SizedE e1) (SizedE e2) =
    case (compare (sizeE e1) (sizeE e2)) of
      EQ -> compare e1 e2
      res -> res

-- | Generates expressions of given size using (a subset) of given operators. 
--   May omit expressions that have shorter equivalents.
--   Fold will only be used if the flag mustfold is set; but if the flag is set it will definitely be used.
--   (It must still be present in the  oplist).
findE :: Size -> [Op] -> InFold -> MustFold -> [E]
findE 1 _   _      True = []
findE 1 _   False  _    = [Id Input, One, Zero]
findE 1 _   True   _    = [Id Input, One, Zero, Id Byte, Id Acc]
findE 2 _   _      True = []

-- XXX Here (n = 2 and above) we ignore mustfold, so it's not guaranteed that
-- *all* results will contain fold. However, if 2 < n < 5, we return no results
-- if mustfold is true. This behavior is inconsistent with the description, so
-- one of the two should be fixed (not sure which).
-- XXX TH: no, everything is fine, mustfold is handled through pattern match
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
                      -- XXX: in all the examples below, we generate all possible values of e1 again for each value of e0 - don't we? That's a waste, fixable by inserting lets.

                        then [ (Op2 op2) e0 e1 |  i <- [5..((n-1) `div` 2)],                   -- optimization: e0 <=  e1, i starts at 5 to allow for fold
                                                 e0 <- findE i       ops  infold True,         -- XXX don't we need e0 /= Zero, since mustfold = True currently doesn't imply we get a program with a fold?
                                                 e1 <- findE (n-1-i) ops' infold False,
                                                 e0 <= e1
                             ]

                          ++ [ (Op2 op2) e0 e1 |  i <- [1..((n-1) `div` 2)],                   -- optimization: e0 <=  e1
                                                 e0 <- findE i       ops' infold False,
                                                 e0 /= Zero,                                   -- prune: 0 binop e always has smaller equivalent. e binop 0 is discarded by the ordering constraint
                                                 e1 <- findE (n-1-i) ops  infold True,
                                                 e0 <= e1
                             ]
                          -- Instead of just |e0| <= |e1|, we use SizedE e0 <= SizedE e1, since E is now instance of Ord. That guarantees that e0 <= e1 implies |e0| <= |e1|, so we can
                          -- first ensure |e0| <= |e1| (by not generating cases violating this invariant, which saves time) and then that

                        else [ (Op2 op2) e0 e1 |  i <- [1..((n-1) `div` 2)],                   -- optimization: e0 <=  e1
                                                 e0 <- findE i       ops infold False,
                                                 e0 /= Zero,                                   -- prune: 0 binop e always has smaller equivalent. e binop 0 is discarded by the ordering constraint
                                                 e1 <- findE (n-1-i) ops infold False,
                                                 e0 <= e1
                             ]

    gen OpIf0       = if mustfold
                        then [ If0 e0 e1 e2 |  i <- [5..n-3], j <-[1..n-2-i], let k = n-1-i-j,
                                              e0 <- findE i ops  infold True,
                                              e1 <- findE j ops' infold False,
                                              e2 <- findE k ops' infold False]
                          ++ [ If0 e0 e1 e2 |  i <- [1..n-7], j <-[5..n-2-i], let k = n-1-i-j,
                                              e0 <- findE i ops' infold False,
                                              e0 /= Zero,                                      -- prune: if0 0 e1 e2 always has smaller equivalent
                                              e0 /= One,                                       -- prune: if0 1 e1 e2 always has smaller equivalent
                                              e1 <- findE j ops  infold True,
                                              e2 <- findE k ops' infold False]
                          ++ [ If0 e0 e1 e2 |  i <- [1..n-7], j <-[1..n-6-i], let k = n-1-i-j,
                                              e0 <- findE i ops' infold False,
                                              e0 /= Zero,                                      -- prune: if0 0 e1 e2 always has smaller equivalent
                                              e0 /= One,                                       -- prune: if0 1 e1 e2 always has smaller equivalent
                                              e1 <- findE j ops' infold False,
                                              e2 <- findE k ops  infold True]
                        else [ If0 e0 e1 e2 |  i <- [1..n-3], j <-[1..n-2-i], let k = n-1-i-j,
                                              e0 <- findE i ops infold False,
                                              e0 /= Zero,                                      -- prune: if0 0 e1 e2 always has smaller equivalent
                                              e0 /= One,                                       -- prune: if0 1 e1 e2 always has smaller equivalent
                                              e1 <- findE j ops infold False,
                                              e2 <- findE k ops infold False]
    gen OpFold      = if mustfold 
                        then (if (n==5) then [Zero, One, Id Input] else [])                     -- prune: fold with constant function reduces to constant
                          ++ [ Fold e0 e1 e2 |  i <- [1..n-4], j <-[1..n-3-i], let k = n-2-i-j,
                                                let newops = delete OpFold ops,
                                                e2 <- findE k newops True  False,   -- e2 before e0 and e1 to prune on it
                                                e2 /= Zero,
                                                e2 /= One,
                                                e2 /= (Id Input),
                                                e0 <- findE i newops False False,
                                                e1 <- findE j newops False False]
                        else []
    -- This shouldn't happen.
    gen other  = traceShow other $ error (show other)

findETopFold :: Size -> [Op] -> [E]
findETopFold n ops = (if (n==5) then [Zero, One, Id Input] else [])                     -- prune: fold with constant function (0, 1, input) reduces to constant
                       ++ [ Fold e0 Zero e2 |  i <- [1..n-4], j <-[1..n-3-i], let k = n-2-i-j,
                                                let newops = delete OpFold ops,
                                                e2 <- findE k newops True  False,   -- e2 before e0 and e1 to prune on it
                                                e2 /= Zero,
                                                e2 /= One,
                                                e2 /= (Id Input),
                                                e0 <- findE i newops False False]

-- number of generated programs for given size using all operators:
-- size  7:    379164
-- size  8:   3278604
-- size  9:  30308787
-- size 10: 276005523
