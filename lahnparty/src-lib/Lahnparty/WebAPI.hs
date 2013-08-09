{-# LANGUAGE PatternGuards #-}

module Lahnparty.WebAPI where

import Numeric

import Data.Word
import Text.JSON


--
-- * Helper Functions
--

-- lookup a required field in a JSON object
lookupReq :: JSON a => [(String, JSValue)] -> String -> Result a
lookupReq m k | Just v <- lookup k m = readJSON v
              | otherwise            = Error ("Missing required field: " ++ k)

-- lookup an optional field in a JSON object
lookupOpt :: JSON a => [(String, JSValue)] -> String -> Result (Maybe a)
lookupOpt m k | Just v <- lookup k m = fmap Just (readJSON v)
              | otherwise            = Ok Nothing

optField :: JSON a => String -> Maybe a -> [(String,JSValue)]
optField k (Just a) = [(k, showJSON a)]
optField _ _        = []


--
-- * JSON Spec
--

data Problem = Problem {
  problem_id        :: String,     -- problem ID
  problem_size      :: Int,        -- problem size |P| [3, 30]
  problem_operators :: [String],   -- set of operators like ["fold", "plus"] (TODO: change to [Op] once we have Tillmann's code?)
  problem_solved    :: Maybe Bool, -- is the problem solved?
  problem_timeLeft  :: Maybe Float -- time we have left
} deriving (Eq,Show)

instance JSON Problem where
  
  readJSON (JSObject o) = do
      id   <- lookupReq m "id"
      size <- lookupReq m "size"
      ops  <- lookupReq m "operators"
      sol  <- lookupOpt m "solved"
      time <- lookupOpt m "timeLeft"
      return (Problem id size ops sol time)
    where m = fromJSObject o
  readJSON _ = Error ""
  
  showJSON (Problem id size ops sol time) =
      JSObject $ toJSObject $ [
        ("id", showJSON id),
        ("size", showJSON size),
        ("operators", showJSON ops)
      ] ++ optField "solved" sol ++ optField "timeLeft" time


-- There are two kinds of eval requests:
--   1. request the results of solution for up to 256 arguments
--   2. compute the results of a program for up to 256 arguments
-- We implement only the first since we can compute the second on our own.
data EvalRequest = EvalRequest {
  evalRequest_id        :: String,  -- program ID like "dKdeIAoZMyb5y3a74iTcLXyr"
  evalRequest_arguments :: [Word64] -- up to 256, 64bit unsigned numbers (output in hex)
} deriving (Eq,Show)

instance JSON EvalRequest where
  
  readJSON (JSObject o) = do
      id   <- lookupReq m "id"
      args <- lookupReq m "arguments" >>= return . map read
      return (EvalRequest id args)
    where m = fromJSObject o

  showJSON (EvalRequest id args) =
      JSObject $ toJSObject $ [
        ("id", showJSON id),
        ("arguments", showJSON (map toHex args))
      ]
    where toHex w = "0x" ++ showHex w ""
    

data EvalResponse = EvalResponse {
  evalResponse_status  :: String,
  evalResponse_outputs :: [String],  
  evalResponse_message :: String
}

data Guess = Guess {
  guess_id      :: String,
  guess_program :: String
}

data GuessResponse = GuessResponse {
  guessResponse_status    :: String,
  guessResponse_values    :: [String],
  guessResponse_message   :: String,
  guessResponse_lightning :: Bool
}

data TrainRequest = TrainRequest {
  trainRequiest_size      :: Int,
  trainRequiest_operators :: [String]
}

data TrainingProblem = TrainingProblem {
  trainingProblem_challenge :: String,
  trainingProblem_id        :: String,
  trainingProblem_size      :: Int,
  trainingProblem_operators :: [String]
}

{-

data Status = Status {
  status_easyChairId    :: Int,
  status_contestScore   :: Int,
  status_lightningScore :: Int,
  status_trainingScore  :: Int,
  status_mismatches     :: Int,
  status_numRequests    :: Int,
  status_requestWindow  :: RequestWindow,
  status_cpuWindow      :: CpuWindow,
  status_cpuTotalTime   :: Int
}

data RequestWindow = RequestWindow {
  requestWindow_resetsIn :: Int,
  requestWindow_amount   :: Int,
  requestWindow_limit    :: Int

}

data CpuWindow = CpuWindow {
  cpuWindow_resetsIn :: Int,
  cpuWindow_amount   :: Int,
  cpuWindow_limit    :: Int
}

-}
