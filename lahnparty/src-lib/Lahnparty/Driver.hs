module Lahnparty.Driver where

import Lahnparty.Language
import Lahnparty.WebAPI
import Lahnparty.GeneratorTH
import Lahnparty.ProblemsDB

driver :: Generator -> ProblemID -> Size -> [Op] -> IO ()
driver gen probId size ops =
  do
    let programs = gen size ops
    mapM_ print $ take 10 programs
    let inputs = randomInputs programs
    result <- evalRequest probId inputs
    case result of
      OK (EvalResponseOK outputs) -> do
        let programsFilt = filterProgs programs inputs outputs
        getMoreInfo probId programsFilt

    return ()

getMoreInfo probId [] = do
  putStrLn "No more possible programs"
  return ()

getMoreInfo probId (p: programs) = do
  putStrLn $ "Guessing program " ++ prettyP p
  res <- guessRequest probId p
  case res of
    OK GuessResponseWin ->
      return ()
    OK (GuessResponseError str) -> do
      putStrLn $ "What? GuessResponseError " ++ str
      getMoreInfo probId programs
    OK (GuessResponseMismatch words) ->
      getMoreInfo probId $ filterProgs programs [words !! 0] [words !! 1]

filterProgs programs inputs outputs =
  [ program | program <- programs,
    (input, output) <- zip inputs outputs,
    evalP input program == output ]
    -- XXX speed this evaluation by SIMD evaluation (?)

randomInputs programs = [0 .. 255]


main = do
    -- let (size, ops) = fetchData probId

  
  let probId = "Qae2h1FwmKd3cTPhFhzSTAKS"
  let (size, ops) = (5, [OpOp1 Not, OpOp2 Plus])
  driver findP probId size ops

-- Ok (TrainingProblem "(lambda (x_3767) (not (plus 1 x_3767)))" "Qae2h1FwmKd3cTPhFhzSTAKS" 5 ["not","plus"])
