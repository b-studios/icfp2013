module Lahnparty.TristateEval where

import Data.Bits
import Data.Word (Word64)
import Numeric

import Lahnparty.Language

-- Unbox/unpack this record.
data TristateWord = T { bits :: Word64, bitMask :: Word64 }
  deriving (Eq, Show)

normalizeTristate (T bits mask) = T (bits .&. mask) mask

-- Combines partially defined numbers. We assume they don't contradict each other.
orDefined t1 @ (T bits1 mask1) t2 @ (T bits2 mask2) = T bitsRes maskRes
  where
    bitsRes = bits1 .&. mask1 .|. bits2 .&. mask2
    maskRes = mask1 .|. mask2

tristateConst v = T v $ complement 0
tristateUndef = T 0 0

instance ProgData TristateWord where
  evalHole = tristateUndef
  zero = tristateConst 0
  one  = tristateConst 1

  evalOp1 = evalOp1Tristate
  evalOp2 = evalOp2Tristate
  evalIf = evalIfTristate

  doShift = shiftOnBothComponents

shiftOnBothComponents shiftFun (T bits mask) = T (shiftFun bits) (shiftFun mask)

evalOp1Tristate :: Op1 -> TristateWord -> TristateWord
-- Not flips bits but keeps definedness.
evalOp1Tristate Not (T bits mask) = T (complement bits) mask
-- Shifts create extra definedness, as zero bits.
evalOp1Tristate shiftOp t =
  orDefined (shiftOnBothComponents (evalOp1 shiftOp) t)
            (T 0 (definedMask shiftOp))


-- | Mask for which bits are definitively zero for a given shift, irrespective of the input.
definedMask :: Op1 -> Word64
definedMask Shl1 = 1
definedMask Not = error "definedMask Not"
-- Set the most significant (shiftAmount shiftOp) bits.
definedMask shiftOp = shiftL (-1) (64 - shiftAmount shiftOp)

evalOp2Tristate
  :: Op2 -> TristateWord -> TristateWord -> TristateWord

-- XXX Plus needs a different logic, return an undefined result
evalOp2Tristate Plus t1 @ (T bits1 mask1) t2 @ (T bits2 mask2) = tristateUndef

-- This evaluates one operation with 6-7 operations.
evalOp2Tristate op2 t1 @ (T bits1 mask1) t2 @ (T bits2 mask2) =
  T (evalOp2 op2 bits1 bits2) (bothDefined .|. maskOp2TristateTwice op2 t1 t2)
  where
    bothDefined = mask1 .&. mask2

    maskOp2TristateTwice Xor t1 t2 = 0
    maskOp2TristateTwice op t1 t2 =
      (maskOp2Tristate op t1) .|.
      (maskOp2Tristate op t2)

    maskOp2Tristate And (T bits1 mask1) = mask1 .&. complement bits1
    maskOp2Tristate Or  (T bits1 mask1) = mask1 .&. bits1

evalIfTristate
  :: TristateWord -> TristateWord -> TristateWord -> TristateWord
evalIfTristate cond @ (T condBits condMask) thenB elseB =
  if condZero then
     thenB
  else if condNonZero then
   elseB
  else
    agree thenB elseB
  where
    condNonZero = (condBits .&. condMask /= 0) -- At least one bit which is set and defined.
    condZero = (complement condBits .&. condMask == -1) -- All bits are unset and defined.

    agree thenB @ (T thenBits thenMask) elseB @ (T elseBits elseMask) =
      T thenBits -- many bits will be garbage.
          -- Check bitwise if both bits are defined and agree
          (thenMask .&. elseMask .&. complement (thenBits `xor` elseBits))
