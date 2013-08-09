module Lahnparty.Driver where

import Lahnparty.Language
import Lahnparty.WebAPI
import Lahnparty.GeneratorTH
import Lahnparty.ProblemsDB

import Text.JSON

driver :: Generator -> ProblemID -> IO ()
driver gen probId =
  do
    let (size, ops) = fetchData probId
    let programs = gen size ops
    let inputs = randomInputs programs
    result <- evalRequest probId inputs
    case result of
      Ok (EvalResponseOK outputs) -> do
        let programsFilt = filterProgs programs inputs outputs
        getMoreInfo probId programsFilt

    return ()

getMoreInfo probId [] =
  return ()
getMoreInfo probId (p: programs) = do
  res <- guessRequest probId p
  case res of
    Ok GuessResponseWin ->
      return ()
    Ok (GuessResponseError str) -> do
      putStrLn "What?"
      getMoreInfo probId programs
    Ok (GuessResponseMismatch words) ->
      -- XXX Not really what we want
      getMoreInfo probId programs

filterProgs programs inputs outputs =
  [ program | program <- programs,
    (input, output) <- zip inputs outputs,
    evalP input program == output ]


randomInputs programs = [0 .. 255] -- undefined
---main :: IO ()
--main = d
  
