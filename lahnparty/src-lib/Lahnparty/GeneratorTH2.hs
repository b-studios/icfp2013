{-# LANGUAGE BangPatterns #-}
module Lahnparty.GeneratorTH2 where

import Lahnparty.Language
import Lahnparty.LanguageUtils
import Lahnparty.TristateEval
import Lahnparty.Types
import Data.Bits
import Data.List(delete)
import qualified Data.Vector as V
import Data.Word (Word64)
import Debug.Trace
import Data.List

type Argument = Word64
type Result = Word64
type Mask = Word64
type InFold = Bool
type MustFold = Bool

------------------

-- | Represents knowledge about the program. Supplied with Argument it should produce a result r
--   such that (r & Mask) == (Result & Mask)
data KnownPoint = Know {-# UNPACK #-} !Mask 
                       {-# UNPACK #-} !Argument 
                       {-# UnPACK #-} !Result
  deriving (Eq,Show)

type Knowledge = V.Vector KnownPoint

know :: Argument -> Result -> KnownPoint
know = Know (0xFFFFFFFFFFFFFFFF)

emptyKnowledge = V.empty

buildKnowledge :: [Word64] -> [Word64] -> Knowledge
buildKnowledge arg res = V.fromList (zipWith know arg res)

isValid :: Knowledge -> Op -> Bool
isValid k (OpOp1 Shl1)  = check k 0 1
isValid k (OpOp1 Shr1)  = check k 0 0x8000000000000000 
isValid k (OpOp1 Shr4)  = check k 0 0xF000000000000000 
isValid k (OpOp1 Shr16) = check k 0 0xFFFF000000000000 
isValid k _ = True  -- there is no constraint for other ops without knowing subexps

check :: Knowledge -> Result -> Mask -> Bool
check k r m = V.all p k
  where
    p (Know mask _ res) = mask .&. m .&. res == mask .&. m .&. r

allBits = 0xFFFFFFFFFFFFFFFF

isValidConst :: Knowledge -> E -> Bool
isValidConst k Zero       = check k 0 allBits
isValidConst k One        = check k 1 allBits
isValidConst k (Id Input) = V.all (\(Know m a r) -> m .&. a == m .&. r) k
isValidConst _ _ = True


adjustOp1 :: Knowledge -> Op -> Knowledge
adjustOp1 k (OpOp1 Not)   = V.map (\(Know m a r) -> Know m a (complement r)) k
adjustOp1 k (OpOp1 Shl1)  = V.map (shiftKnowR 1) k
adjustOp1 k (OpOp1 Shr1)  = V.map (shiftKnowL 1) k
adjustOp1 k (OpOp1 Shr4)  = V.map (shiftKnowL 4) k
adjustOp1 k (OpOp1 Shr16) = V.map (shiftKnowL 16) k

shiftKnowR :: Int -> KnownPoint -> KnownPoint
shiftKnowR n (Know m a r) = Know (shiftR m n) a (shiftR r n)

shiftKnowL :: Int -> KnownPoint -> KnownPoint
shiftKnowL n (Know m a r) = Know (shiftL m n) a (shiftL r n)


adjustForFst :: Knowledge -> Op -> Knowledge
adjustForFst k (OpOp2 And) = V.map (\(Know m a r) -> Know (m .&. r) a (m .&. r)) k
adjustForFst k (OpOp2 Or)  = V.map (\(Know m a r) -> Know (m .&. (complement r)) a 0) k
adjustForFst k _ = emptyKnowledge


adjustForSnd :: Knowledge -> Op -> E -> Knowledge
-- adjustForSnd _ (OpOp2 Plus) _ = emptyKnowledge
adjustForSnd k op e = --old: adjustForFst k op -- #todo: partial evaluate E and use that info! --done!
                      V.map (adjustForSnd' op e) k -- do not delete empty KnownPoints ( where mask is 0)... we might regain information and then need the data point

adjustForSnd' :: Op -> E -> KnownPoint -> KnownPoint
adjustForSnd' (OpOp2 And) e p@(Know m a r) = let (Know m1 _ v1) = evalPart e p
                                             in Know (m .&. m1 .&. v1) a (v1 .&. r)
adjustForSnd' (OpOp2 Or) e p@(Know m a r)  = let (Know m1 _ v1) = evalPart e p
                                             in Know (m .&. m1 .&. (complement v1)) a r
adjustForSnd' (OpOp2 Xor) e p@(Know m a r)  = let (Know m1 _ v1) = evalPart e p
                                              in Know (m .&. m1) a (v1 `xor` r)
adjustForSnd' (OpOp2 Plus) e p@(Know m a r)  = let p1 @ (Know m1 _ v1) = evalPart e p
                                                   -- Here, the initial carry must be 1!
                                                   hasCarryMask   = (complement r) .&. v1
                                                   -- Here, the initial carry must be 0!
                                                   hasNoCarryMask = r .&. (complement v1)
                                                   carryMask = (shiftL (hasCarryMask .|. hasNoCarryMask) 1) .|. 1
                                                   carryVal = shiftL hasCarryMask 1
                                                   finalCarry =
                                                     -- Know carryMask 0 carryVal
                                                     computeCarry p1 (Know carryMask 0 carryVal) p
                                               in computeV2 p1 finalCarry p

computeV2 :: KnownPoint -> KnownPoint -> KnownPoint -> KnownPoint
computeV2 v1@(Know v1Mask _ v1Val) carry@(Know carryMask _ carryVal) res@(Know resMask a resVal)
  = let knowAllMask         = (v1Mask .&. carryMask .&. resMask) --all three known 
        knowAllNewV2Val     = (resVal `xor` v1Val `xor` carryVal)
{-
        knowCRMask          = ((complement v1Mask) .&. carryMask .&. resMask) --know carry and res
        -- knowCRNewCarryMask  = (carryMask `xor`resMask)
        -- knowCRNewCarryVal   = carryVal

        knowVRMask          = (v1Mask .&. (complement carryMask) .&. resMask) --know v1 and res
        -- knowVRNewCarryMask  = (v1Mask `xor` resMask)
        -- knowVRNewCarryVal   = v1Val

        knowVCMask          = (v1Mask .&. carryMask .&. (complement resMask)) --know v1 and carry
        -- knowVCNewCarryMask  = complement (v1Mask `xor` carryMask)
        -- knowVCNewCarryVal   = v1Val
-}
    in Know knowAllMask a knowAllNewV2Val

-- Fixpoint loop to compute carry more precisely.

computeCarry :: KnownPoint -> KnownPoint -> KnownPoint -> KnownPoint
computeCarry v1@(Know v1Mask _ v1Val) carry@(Know carryMask _ carryVal) res@(Know resMask _ resVal)
  = let !knowAllMask         = (v1Mask .&. carryMask .&. resMask) --all three known 
        -- knowAllNewCarryVal  = ((v1Mask .&. carryMask) .|. (v1Mask .&. resMask) .|. (carryMask .&. resMask)) 
        !knowAllNewCarryVal  = ((complement resVal .&. (v1Val .|. carryVal)) .|. (resVal .&. v1Val .&. carryVal)) .&. knowAllMask

        !knowCRMask          = ((complement v1Mask) .&. carryMask .&. resMask) --know carry and res
        !knowCRNewCarryMask  = (carryMask `xor`resMask) .&. knowCRMask
        !knowCRNewCarryVal   = carryVal .&. knowCRNewCarryMask

        !knowVRMask          = (v1Mask .&. (complement carryMask) .&. resMask) --know v1 and res
        !knowVRNewCarryMask  = (v1Mask `xor` resMask) .&. knowVRMask
        !knowVRNewCarryVal   = v1Val .&. knowVRNewCarryMask

        !knowVCMask          = (v1Mask .&. carryMask .&. (complement resMask)) --know v1 and carry
        !knowVCNewCarryMask  = (complement (v1Mask `xor` carryMask)) .&. knowVCMask
        !knowVCNewCarryVal   = v1Val .&. knowVCNewCarryMask

        !carryMask' = (shiftL (knowAllMask .|. knowCRNewCarryMask .|. knowVRNewCarryMask .|. knowVCNewCarryMask) 1) .|. carryMask
        !carryVal'  = (shiftL (knowAllNewCarryVal .|. knowCRNewCarryVal .|. knowVRNewCarryVal .|. knowVCNewCarryVal) 1) .|. carryVal
--    in if carryVal == carryVal'
    in if carryMask == carryMask'
      then carry
      else computeCarry v1 (Know carryMask' 0 carryVal') res


-- #todo: partial evaluate E and use that info!
adjustForIf0Then :: Knowledge -> E -> Knowledge
adjustForIf0Then k e = V.filter shouldKeep k
  where
    shouldKeep :: KnownPoint -> Bool
    shouldKeep p@(Know m _ r) = let (Know mask _ condVal) = evalPart e p
                                in (mask == allBits && condVal == 0)

adjustForIf0Else :: Knowledge -> E -> Knowledge
adjustForIf0Else k e = V.filter shouldKeep k
  where
    shouldKeep :: KnownPoint -> Bool
    shouldKeep p@(Know m _ r) = let (Know mask _ condVal) = evalPart e p
                                in ((mask .&. condVal) /= 0)




evalPart :: E -> KnownPoint -> KnownPoint
evalPart e (Know m a r) = let (T r' m') = evalEGen (T a allBits) (T 0 0) (T 0 0) e
                          in (Know m' a r')

------------------


generate :: Int -> [Op] -> [(Argument, Result)] -> [P]
generate size ops points = undefined


type Generator = [Size] -> [Op] -> Knowledge -> [P]

findP :: Generator
findP sizes ops known =
  if OpTFold `elem` ops'
   then
     -- assertFalse (OpFold `elem` ops)

     -- XXX This findE should produce a fold at the top level
     -- map Lambda $ findETopFold (size - 1) (delete OpTFold ops)
     map Lambda $ concatMap (\s -> findETopFold s (delete OpTFold ops') known) (filter (>= 5) sizes)
   else
     --map Lambda $ findE (size - 1) ops False (OpFold `elem` ops)
     map Lambda $ concatMap (\s -> findE s ops' False (OpFold `elem` ops') known) sizes
  where
    ops' = sort ops


genOp1 ops n infold mustfold op1 known =
  [ Op1 op1 e0
  | let newknown = adjustOp1 known (OpOp1 op1),
    e0 <- findE (n-1) ops infold mustfold newknown,
    not (isShift op1 && e0 == Zero) -- shifts on Zero always have smaller equivalents. PG
  ]

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
findE :: Size -> [Op] -> InFold -> MustFold -> Knowledge -> [E]
findE 1 _   _      True _     = []
findE 1 _   False  _    known = filter (isValidConst known) [Id Input, One, Zero]
findE 1 _   True   _    known = filter (isValidConst known) [Id Input, One, Zero, Id Byte, Id Acc]
findE 2 _   _      True _     = []

findE n@2 ops infold _ !known  = let ops1 = map (\(OpOp1 op) -> op) $ filter (isValid known) $ filter isOp1 ops 
                                 in concatMap gen ops1
  where
    gen op = genOp1 ops n infold False op known

findE n ops infold mustfold !known = if (n<5 && mustfold) 
                                      then []
                                      else concatMap gen (filter (isValid known) ops)
  where
    ops' = delete OpFold ops

    gen :: Op -> [E]
    gen (OpOp1 op1) = genOp1 ops n infold mustfold op1 known
    gen op@(OpOp2 op2) 
                    = if mustfold
                        then [ (Op2 op2) e0 e1 |  i <- [5..((n-1) `div` 2)],                   -- optimization: e0 <=  e1, i starts at 5 to allow for fold
                                                 e0 <- findE i       ops  infold True (adjustForFst known op),   -- XXX don't we need e0 /= Zero, since mustfold = True currently doesn't imply we get a program with a fold?
                                                 e1 <- findE (n-1-i) ops' infold False (adjustForSnd known op e0),
                                                 e0 <= e1
                             ]

                          ++ [ (Op2 op2) e0 e1 |  i <- [1..((n-1) `div` 2)],                   -- optimization: e0 <=  e1
                                                 e0 <- findE i       ops' infold False (adjustForFst known op),
                                                 e0 /= Zero,                                   -- prune: 0 binop e always has smaller equivalent. e binop 0 is discarded by the ordering constraint
                                                 e1 <- findE (n-1-i) ops  infold True (adjustForSnd known op e0),
                                                 e0 <= e1
                             ]
                          -- Instead of just |e0| <= |e1|, we use SizedE e0 <= SizedE e1, since E is now instance of Ord. That guarantees that e0 <= e1 implies |e0| <= |e1|, so we can
                          -- first ensure |e0| <= |e1| (by not generating cases violating this invariant, which saves time) and then that

                        else [ (Op2 op2) e0 e1 |  i <- [1..((n-1) `div` 2)],                   -- optimization: e0 <=  e1
                                                 e0 <- findE i       ops infold False (adjustForFst known op),
                                                 e0 /= Zero,                                   -- prune: 0 binop e always has smaller equivalent. e binop 0 is discarded by the ordering constraint
                                                 e1 <- findE (n-1-i) ops infold False (adjustForSnd known op e0),
                                                 e0 <= e1
                             ]

    gen OpIf0       = if mustfold
                        then [ If0 e0 e1 e2 |  i <- [5..n-3], j <-[1..n-2-i], let k = n-1-i-j,
                                              e0 <- findE i ops  infold True emptyKnowledge,
                                              e1 <- findE j ops' infold False (adjustForIf0Then known e0),
                                              e2 <- findE k ops' infold False (adjustForIf0Else known e0)]
                          ++ [ If0 e0 e1 e2 |  i <- [1..n-7], j <-[5..n-2-i], let k = n-1-i-j,
                                              e0 <- findE i ops' infold False emptyKnowledge,
                                              e0 /= Zero,                                      -- prune: if0 0 e1 e2 always has smaller equivalent
                                              e0 /= One,                                       -- prune: if0 1 e1 e2 always has smaller equivalent
                                              e1 <- findE j ops  infold True (adjustForIf0Then known e0),
                                              e2 <- findE k ops' infold False (adjustForIf0Else known e0)]
                          ++ [ If0 e0 e1 e2 |  i <- [1..n-7], j <-[1..n-6-i], let k = n-1-i-j,
                                              e0 <- findE i ops' infold False emptyKnowledge,
                                              e0 /= Zero,                                      -- prune: if0 0 e1 e2 always has smaller equivalent
                                              e0 /= One,                                       -- prune: if0 1 e1 e2 always has smaller equivalent
                                              e1 <- findE j ops' infold False (adjustForIf0Then known e0),
                                              e2 <- findE k ops  infold True (adjustForIf0Else known e0)]
                        else [ If0 e0 e1 e2 |  i <- [1..n-3], j <-[1..n-2-i], let k = n-1-i-j,
                                              e0 <- findE i ops infold False emptyKnowledge,
                                              e0 /= Zero,                                      -- prune: if0 0 e1 e2 always has smaller equivalent
                                              e0 /= One,                                       -- prune: if0 1 e1 e2 always has smaller equivalent
                                              e1 <- findE j ops infold False (adjustForIf0Then known e0),
                                              e2 <- findE k ops infold False (adjustForIf0Else known e0)]
    gen OpFold      = if mustfold 
                        then (if (n==5) then [Zero, One, Id Input] else [])                     -- prune: fold with constant function reduces to constant
                          ++ [ Fold e0 e1 e2 |  i <- [1..n-4], j <-[1..n-3-i], let k = n-2-i-j,
                                                let newops = delete OpFold ops,
                                                e2 <- findE k newops True  False emptyKnowledge,   -- e2 before e0 and e1 to prune on it
                                                e2 /= Zero,
                                                e2 /= One,
                                                e2 /= (Id Input),
                                                e0 <- findE i newops False False emptyKnowledge,
                                                e1 <- findE j newops False False emptyKnowledge]
                        else []
    -- This shouldn't happen.
    gen other  = traceShow other $ error (show other)

findETopFold :: Size -> [Op] -> Knowledge -> [E]
findETopFold n ops known 
                   = (if (n==5) then [Zero, One, Id Input] else [])                     -- prune: fold with constant function (0, 1, input) reduces to constant
                       ++ [ Fold e0 Zero e2 |  i <- [1..n-4], j <-[1..n-3-i], let k = n-2-i-j,
                                                let newops = delete OpFold ops,
                                                e2 <- findE k newops True  False emptyKnowledge,   -- e2 before e0 and e1 to prune on it
                                                e2 /= Zero,
                                                e2 /= One,
                                                e2 /= (Id Input),
                                                e0 <- findE i newops False False emptyKnowledge]

-- number of generated programs for given size using all operators:
-- size  7:    379164
-- size  8:   3278604
-- size  9:  30308787
-- size 10: 276005523
