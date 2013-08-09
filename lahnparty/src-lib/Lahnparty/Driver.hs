module Lahnparty.Driver where

import Lahnparty.Language
import Lahnparty.WebAPI
import Lahnparty.GeneratorTH
import Lahnparty.ProblemsDB

import Text.JSON

driver :: Generator -> ProblemID -> Size -> [Op] -> IO ()
driver gen probId size ops =
  do
    let programs = gen size ops
    let inputs = randomInputs programs
    result <- evalRequest probId inputs
    case result of
      Ok (EvalResponseOK outputs) -> do
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
    Ok GuessResponseWin ->
      return ()
    Ok (GuessResponseError str) -> do
      putStrLn "What? GuessResponseError " ++ str
      getMoreInfo probId programs
    Ok (GuessResponseMismatch words) ->
      getMoreInfo probId $ filterProgs programs [words !! 0] [words !! 1]

filterProgs programs inputs outputs =
  [ program | program <- programs,
    (input, output) <- zip inputs outputs,
    evalP input program == output ]
    -- XXX speed this evaluation by SIMD evaluation (?)

randomInputs programs = [0 .. 255]

{-
main = do
    -- let (size, ops) = fetchData probId

    let probId = "Qae2h1FwmKd3cTPhFhzSTAKS"
    let (size, ops) = (5, [])
    driver findP probId size ops

-}
