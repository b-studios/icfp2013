module Lahnparty.Driver where

import Lahnparty.Language
import Lahnparty.WebAPI
import Lahnparty.GeneratorTH
import Lahnparty.ProblemsDB
import Lahnparty.Types

driver :: Generator -> ProblemID -> Size -> [Op] -> IO ()
driver gen probId size ops =
  do
    let programs = gen size ops
    -- XXX remove in production
    putStrLn $ "# generated programs: " ++ show (length programs)
    putStrLn $ "First 10 generated programs:"

    mapM_ print $ take 10 programs

    let inputs = randomInputs programs
    result <- evalRequest probId inputs
    case result of
      OK (EvalResponseOK outputs) -> do
        let programsFilt = filterProgs programs inputs outputs
        -- XXX remove in production
        putStrLn $ "# generated programs after filtering: " ++ show (length programsFilt)
        putStrLn $ "First 10 generated programs after filtering:"

        mapM_ print $ take 10 programsFilt

        getMoreInfo probId programsFilt
      err -> do
        print err

    return ()

getMoreInfo probId [] = do
  putStrLn "No more possible programs"
  return ()

getMoreInfo probId (p: programs) = do
  putStrLn $ "Guessing program " ++ prettyP p
  res <- guessRequest probId p
  case res of
    OK GuessResponseWin -> do
      putStrLn "We won!"
      return ()
    OK (GuessResponseError str) -> do
      putStrLn $ "What? GuessResponseError " ++ str
      getMoreInfo probId programs
    OK (GuessResponseMismatch words) -> do
      let programsFilt = filterProgs programs [words !! 0] [words !! 1]
      putStrLn $ "# generated programs after filtering on counterexample: " ++ show (length programsFilt)
      getMoreInfo probId programsFilt

filterProgs programs inputs outputs =
  [ program
  | program <- programs
  -- XXX use parallel list comprehensions
  , and [ evalP input program == output
        | (input, output) <- zip inputs outputs
        ]
  ]
    -- XXX speed this evaluation by SIMD evaluation (?)

randomInputs programs = [0 .. 255]


main = do
  let size = 5
  -- trainRequestSizeOps size [] []
-- > Ok (TrainingProblem "(lambda (x_3767) (not (plus 1 x_3767)))" "Qae2h1FwmKd3cTPhFhzSTAKS" 5 ["not","plus"])

  -- let (size, ops) = fetchData probId

  let probId = "Qae2h1FwmKd3cTPhFhzSTAKS"
  let ops = [OpOp1 Not, OpOp2 Plus]
  driver findP probId size ops
