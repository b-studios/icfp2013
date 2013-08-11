{-# LANGUAGE TupleSections #-}

module Lahnparty.WorkerDriver where

import Control.Concurrent (threadDelay)
import Control.Monad (liftM3)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.JSON (JSON)
import Text.Read (readMaybe)

import Lahnparty.Driver hiding (main)
import Lahnparty.Language
import Lahnparty.ProblemsDB
import Lahnparty.Types
import Lahnparty.WebAPI

import qualified Lahnparty.GeneratorTH  as OldGen
import qualified Lahnparty.GeneratorTH2 as NewGen
import Lahnparty.GeneratorTH2 (Generator)

--
-- * Distributed Worker Driver
--

data Work = Work ProblemID Size [Op] Int Int
  deriving (Eq,Show)

-- | Run a training worker.
--   NOTE: The server must also be in training mode!
runTrainWorker :: WorkerID -> Generator -> IO ()
runTrainWorker = runWorker work
  where
    work (DistTrainingProblem prob wnum wtot) = 
      let (pid,size,ops) = parseTrainingData (OK prob)
      in Work pid size ops wnum wtot

-- | Run a live worker.
--   NOTE: The server must also be in live mode!
runLiveWorker :: WorkerID -> Generator -> IO ()
runLiveWorker = runWorker work
  where 
    work (DistProblem pid wnum wtot) = 
      let (size,ops) = fetchData pid
      in Work pid size ops wnum wtot

-- | Generic worker driver.
runWorker :: (Show a, JSON a) => (a -> Work) -> WorkerID -> Generator -> IO ()
runWorker work wid g = do
    
    response <- registerWorker wid
    print response
    
    case response of
      
      OK result -> do
        let Work pid size ops wnum wtot = work result
        -- TODO split up work here!
        distDriver wid g pid size ops
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
      runWorker work wid g

main :: IO ()
main = do
    (run,gen,wid) <- getArgs >>= readArgs
    run wid gen
  where 
    
    readArgs [s,g,i] = liftM3 (,,) (readSet s) (readGen g) (readID i)
    readArgs _ = usage
    
    readSet "live"  = return runLiveWorker
    readSet "train" = return runTrainWorker
    readSet _ = usage
    
    readGen "old"   = return OldGen.findP
    readGen "new"   = return NewGen.findP
    readGen _ = usage
    
    readID = maybe usage return . readMaybe
    
    usage = do 
      putStrLn "lahnparty-run-worker {live|train} {old|new} wid"
      putStrLn "  live  - run on live problems"
      putStrLn "  train - run on training problems"
      putStrLn "  old   - use old generator"
      putStrLn "  new   - use new generator"
      putStrLn "  wid   - an integer unique to this worker"
      exitFailure
