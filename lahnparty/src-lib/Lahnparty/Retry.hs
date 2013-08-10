-- eval & retry until success/failure

module Lahnparty.Retry (retry) where

import Control.Concurrent (threadDelay)
import Control.Monad
import System.Exit (exitFailure)
import Data.Bits (rotate, complement)
import Data.List (nub)
import Data.Set hiding (map, filter)
import Data.Word (Word64)
import Data.Random.Source
import Data.Random.Source.IO

import Lahnparty.Language
import Lahnparty.WebAPI
import Lahnparty.GeneratorTH
import Lahnparty.ProblemsDB
import Lahnparty.Types
import Lahnparty.Driver

maxEvalInputs :: Int
maxEvalInputs = 256

retry :: ProblemID -> [P] -> IO ()
retry = getMoreInfo

retryWithEval :: ProblemID -> [P] ->
  Set Word64 -> Word64 -> Word64 -> IO ()

retryWithEval problemID programs triedInputs mismatchIn mismatchOut =
  do
    (randoms, setToAvoid) <- getNewInputs mismatchIn triedInputs
    error "???"

getNewInputs :: Word64 -> Set Word64 -> IO ([Word64], Set Word64)
getNewInputs mismatchIn setToAvoid =
  do
    let (preset, presetAvoided) = preassign mismatchIn setToAvoid
        remaining = maxEvalInputs - length preset
    (randoms, randomsAvoided) <- genRandomInput remaining presetAvoided
    return (preset ++ randoms, randomsAvoided)

-- when a counter example is given, we test it against
-- all rotations of the counter example's input except
-- those tested already.
preassign :: Word64 -> Set Word64 -> ([Word64], Set Word64)
preassign mismatchIn setToAvoid =
  let
    preset = filter (\ x -> not (member x setToAvoid)) $ nub $
      complement mismatchIn :
      map (rotate mismatchIn) [1..63]
  in
    (preset, union (fromList preset) setToAvoid)

-- input: number of Word64 to generate, the set to avoid
-- output: list of generated words, new set to avoid
genRandomInput :: Int -> Set Word64 -> IO ([Word64], Set Word64)
genRandomInput n setToAvoid = do loop n setToAvoid [] where
  loop :: Int -> Set Word64 -> [Word64] -> IO ([Word64], Set Word64)
  loop n setToAvoid accumulator
    | n <= 0 = return (accumulator, setToAvoid)
    | otherwise = do
      x <- getNext
      return (x : accumulator, insert x setToAvoid)
      where
      getNext :: IO Word64
      getNext = do
        x <- getRandomWord64
        if member x setToAvoid
          then getNext
          else return x
