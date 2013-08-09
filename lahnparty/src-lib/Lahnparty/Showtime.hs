
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

-- Time to wait between problems in seconds.
wait = 10

solveProblemsOfSize :: Generator -> Int -> IO ()
solveProblemsOfSize g n = mapM_ solveProblem (sizeToIDs n)
  where
    solveProblem i = do
      let (size,ops) = fetchData i
      driver g i size ops
      threadDelay (wait * 1000000)

solveProblemsOfSizeFromTo :: Generator -> Int -> Int -> IO ()
solveProblemsOfSizeFromTo g from to = mapM_ (solveProblemsOfSize g) [from .. to]

main = solveProblemsOfSizeFromTo findP 3 8
