
---------------------
-- !!! WARNING !!! --
---------------------
--
-- The functions in this file will submit live eval and guess requests!
--
module Lahnparty.Showtime where

import Control.Concurrent (threadDelay)

import Lahnparty.Driver
import Lahnparty.GeneratorTH
import Lahnparty.ProblemsDB
import Lahnparty.Types

-- Time to wait between problems in seconds.
wait = 3

solveProblems :: Generator -> [ProblemID] -> IO ()
solveProblems g ids = mapM_ solveProblem ids
  where
    solveProblem i = do
      let (size,ops) = fetchData i
      driver g i size ops
      threadDelay (wait * 1000000)

solveProblemsOfSize :: Generator -> Int -> IO ()
solveProblemsOfSize g = solveProblems g . sizeToIDs
      
solveProblemsOfSizeFiltered :: Generator -> Int -> (ProblemID -> Bool) -> IO ()
solveProblemsOfSizeFiltered g n p = solveProblems g (filter p (sizeToIDs n))

solveProblemsInRange :: Generator -> Int -> Int -> IO ()
solveProblemsInRange g from to = mapM_ (solveProblemsOfSize g) [from .. to]

main = solveProblemsOfSizeFiltered findP 8 hasNoFold
