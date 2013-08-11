
module Lahnparty.WorkerDriver where

import Control.Concurrent (threadDelay)
import Text.JSON (JSON)

import Lahnparty.Driver
import Lahnparty.GeneratorTH2
import Lahnparty.Language
import Lahnparty.ProblemsDB
import Lahnparty.Types
import Lahnparty.WebAPI

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
