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

fetchTrainingData :: Size -> IO (ProblemID, Size, [Op])
fetchTrainingData size = do
  OK (TrainingProblem _ id size operators) <- trainRequestSize size
  return (id, size, map opStringToOp operators)

opStringToOp "if0" = OpIf0
opStringToOp "fold" = OpFold
opStringToOp "tfold" = OpTFold
opStringToOp "not" = OpOp1 Not
opStringToOp "shl1" = OpOp1 Shl1
opStringToOp "shr1" = OpOp1 Shr1
opStringToOp "shr4" = OpOp1 Shr4
opStringToOp "shr16" = OpOp1 Shr16
opStringToOp "and" = OpOp2 And
opStringToOp "or" = OpOp2 Or
opStringToOp "xor" = OpOp2 Xor
opStringToOp "plus" = OpOp2 Plus

main = do
  (probId, size, ops) <- fetchTrainingData 5
  driver findP probId size ops

