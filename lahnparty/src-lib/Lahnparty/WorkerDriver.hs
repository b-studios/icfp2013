{-# LANGUAGE ParallelListComp, TupleSections #-}

module Lahnparty.WorkerDriver where

import Control.Concurrent (threadDelay)
import Control.Monad (liftM2)
import Data.List (sort)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import Text.JSON (JSON)

import Lahnparty.Driver hiding (main)
import Lahnparty.Language
import Lahnparty.ProblemsDB
import Lahnparty.Types
import Lahnparty.WebAPI

import qualified Lahnparty.GeneratorTH  as OldGen
import qualified Lahnparty.GeneratorTH2 as NewGen
import qualified Lahnparty.GeneratorBonus42  as BonusGen
import Lahnparty.GeneratorTH2 (Generator)

--
-- * Distributed Worker Driver
--

data Work = Work ProblemID Size [Op] Int Int
  deriving (Eq,Show)

-- | Run a training worker.
--   NOTE: The server must also be in training mode!
runTrainWorker :: WorkerID -> IO ()
runTrainWorker = runWorker work
  where
    work (DistTrainingProblem prob wnum wtot) = 
      let (pid,size,ops) = parseTrainingData (OK prob)
      in Work pid size ops wnum wtot

-- | Run a live worker.
--   NOTE: The server must also be in live mode!
runLiveWorker :: WorkerID -> IO ()
runLiveWorker = runWorker work
  where 
    work (DistProblem pid wnum wtot) = 
      let (size,ops) = fetchData pid
      in Work pid size ops wnum wtot

-- | Generic worker driver.
runWorker :: (Show a, JSON a) => (a -> Work) -> WorkerID -> IO ()
runWorker work wid = do
    
    response <- registerWorker wid
    print response
    
    case response of
      
      OK result -> do
        let Work pid size ops wnum wtot = work result
        let (gen, wnum', wtot') = (NewGen.findP, wnum, wtot) -- chooseGenerator size wnum wtot
        let sizes = chooseSizeRange size wnum' wtot'
        distDriver wid ((if wnum == 1 then BonusGen.addHardcoded else id) gen) pid sizes ops
        sleepThenTryAgain 3
      
      HTTPError (4,2,3) msg -> do
        putStrLn "Pool of workers is full."
        sleepThenTryAgain 10
      
      _ -> do
        putStrLn $ "Unexpected response: " ++ show response
        sleepThenTryAgain 10
  
  where
    sleepThenTryAgain wait = do
      putStrLn "Sleeping..."
      threadDelay (wait * 1000000)
      runWorker work wid

-- TODO maybe this breaks with an odd number of workers
chooseGenerator :: Size -> Int -> Int -> (Generator,Int,Int)
chooseGenerator size num total 
    | num > k   = (NewGen.findP, num-k, total-k)
    | otherwise = (OldGen.findP, num, k)
  where k = (total+1) `div` 2

chooseSizeRange :: Size -> Int -> Int -> [Size]
chooseSizeRange size num total =
  sort $ (partition total [size-1,size-2 .. 1] ++ repeat []) !! (num-1)

partition :: Int -> [Size] -> [[Size]]
partition 1 ss = [ss]
partition n ss =
  [b:bs | b <- take n ss
        | bs <- [] : partition (n-1) (reverse (drop n ss)) ++ repeat []]

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    (run,wid) <- getArgs >>= readArgs
    run wid
  where 
    
    readArgs [s,i] = liftM2 (,) (readSet s) (readID i)
    readArgs _ = usage
    
    readSet "live"  = return runLiveWorker
    readSet "train" = return runTrainWorker
    readSet _ = usage
    
    -- readID = maybe usage return . readMaybe
    readID = return . read
    
    usage = do 
      putStrLn "lahnparty-run-worker {live|train} wid"
      putStrLn "  live  - run on live problems"
      putStrLn "  train - run on training problems"
      putStrLn "  wid   - an integer unique to this worker"
      exitFailure
