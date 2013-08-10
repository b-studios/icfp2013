{-# LANGUAGE PatternGuards, DoAndIfThenElse #-}

module Lahnparty.WebAPI where

import Numeric

import Data.Word (Word64)
import Text.JSON
import Network.HTTP (ResponseCode,postRequestWithBody,simpleHTTP)
import qualified Network.HTTP as HTTP

import Lahnparty.Language
import Lahnparty.Types


--
-- * Public Interface
--

-- | Generic response to a request. Separates the various kinds of errors.
data Response a =
    OK a
  | ConnectionError String
  | HTTPError ResponseCode String
  | JSONError String
  deriving (Eq,Show)


-- ** Submitting Evaluation Requests

data EvalResponse = 
    EvalResponseOK (Maybe [Word64]) [Word64]
  | EvalResponseError String
  deriving (Eq,Show)

-- | Send an evaluation request.
--   Note that there are two kinds of eval requests:
--     1. request the results of solution for up to 256 arguments
--     2. compute the results of a program for up to 256 arguments
--   We implement only the first since we can compute the second on our own.
evalRequest :: ProblemID -> [Word64] -> IO (Response EvalResponse)
evalRequest pid vs = performRequest "eval" (EvalRequest Nothing pid vs)


-- ** Submitting Guesses

data GuessResponse =
    GuessResponseWin
  | GuessResponseMismatch [Word64]
  | GuessResponseError String
  deriving (Eq,Show)

-- | Send a guess request.
guessRequest :: ProblemID -> P -> IO (Response GuessResponse)
guessRequest id = guessRequestString id . prettyP

-- | Send a guess request with a program represented as a string.
guessRequestString :: ProblemID -> String -> IO (Response GuessResponse)
guessRequestString id s = performRequest "guess" (Guess id s)


-- ** Submitting a Training Request

data TrainOps = TrainNone | TrainFold | TrainTFold
  deriving (Eq,Show)

-- TODO: Parse program string and op lists?
data TrainingProblem = TrainingProblem String ProblemID Int [String]
  deriving (Eq,Show)

-- | Send a request for an arbitrary training problem.
trainRequest :: IO (Response TrainingProblem)
trainRequest = performRequest "train" (TrainRequest Nothing Nothing)

-- | Send a request for a training problem of a specified size.
trainRequestSize :: Size -> IO (Response TrainingProblem)
trainRequestSize n = performRequest "train" (TrainRequest (Just n) Nothing)

-- | Send a request for a training problem of a specified size
--   with specified ops.
trainRequestSizeOps :: Size -> TrainOps -> IO (Response TrainingProblem)
trainRequestSizeOps n fold =
    performRequest "train" (TrainRequest (Just n) (Just ops))
  where ops = case fold of TrainNone  -> []
                           TrainFold  -> ["fold"]
                           TrainTFold -> ["tfold"]


-- ** Interface to Distributed Solver

-- | Description of a subset of a problem to work on.
data DistProblem = DistProblem ProblemID Int Int
  deriving (Eq,Show)

-- | Description of a subset of a problem to work on.
data DistTrainingProblem = DistTrainingProblem TrainingProblem Int Int
  deriving (Eq,Show)

-- | Register as a worker.
registerWorker :: WorkerID -> IO (Response DistProblem)
registerWorker = performRequest "register" . RegisterRequest

-- | Send an evaluation request as a distributed worker.
distEvalRequest :: WorkerID -> ProblemID -> [Word64] -> IO (Response EvalResponse)
distEvalRequest wid pid vs = performRequest "eval" (EvalRequest (Just wid) pid vs)

-- | Send a training request as a distributed worker.
distTrainRequest :: WorkerID -> Size -> TrainOps -> IO (Response DistTrainingProblem)
distTrainRequest wid n fold =
    performRequest "train" (TrainRequest (Just n) (Just ops))
  where ops = case fold of TrainNone  -> []
                           TrainFold  -> ["fold"]
                           TrainTFold -> ["tfold"]


--
-- * Internal Code
--

-- ** Helper Functions

-- | Lookup a required field in a JSON object
lookupReq :: JSON a => [(String, JSValue)] -> String -> Result a
lookupReq m k | Just v <- lookup k m = readJSON v
              | otherwise            = Error ("Missing required field: " ++ k)

-- | Lookup an optional field in a JSON object
lookupOpt :: JSON a => [(String, JSValue)] -> String -> Result (Maybe a)
lookupOpt m k | Just v <- lookup k m = fmap Just (readJSON v)
              | otherwise            = Ok Nothing

-- | Generate an optional field for a JSON object
optField :: JSON a => String -> Maybe a -> [(String,JSValue)]
optField k (Just a) = [(k, showJSON a)]
optField _ _        = []

-- | Convert unsigned 64 bit int to hex string
toHex :: Word64 -> String
toHex w = "0x" ++ showHex w ""


-- ** JSON Support Code

data EvalRequest = EvalRequest (Maybe WorkerID) ProblemID [Word64]
  deriving (Eq,Show)

instance JSON EvalRequest where
  
  readJSON (JSObject o) = do
      id   <- lookupReq m "id"
      args <- lookupReq m "arguments" >>= return . map read
      wid  <- lookupOpt m "workerID"
      return (EvalRequest wid id args)
    where m = fromJSObject o
  readJSON _ = Error "Error reading EvalRequest (not JSObject)."

  showJSON (EvalRequest wid id args) =
      JSObject $ toJSObject $ [
        ("id", showJSON id),
        ("arguments", showJSON (map toHex args))
      ] ++ optField "workerID" wid
    

instance JSON EvalResponse where
  
  readJSON (JSObject o) = do
      status <- lookupReq m "status"
      if status == "ok" then do
        outs <- lookupReq m "outputs"   >>= return . map read
        args <- lookupOpt m "arguments" >>= return . fmap (map read)
        return (EvalResponseOK args outs)
      else do
        msg <- lookupReq m "message"
        return (EvalResponseError msg)
    where m = fromJSObject o
  readJSON _ = Error "Error reading EvalResponse (not JSObject)."

  showJSON (EvalResponseOK args outs) =
    JSObject $ toJSObject $ [
      ("status", showJSON "ok"),
      ("outputs", showJSON (map toHex outs))
    ] ++ optField "arguments" args
  showJSON (EvalResponseError msg) =
    JSObject $ toJSObject [
      ("status", showJSON "error"),
      ("outputs", showJSON msg)
    ]


data Guess = Guess ProblemID String
  deriving (Eq,Show)

instance JSON Guess where
  
  readJSON (JSObject o) = do
      id   <- lookupReq m "id"
      prog <- lookupReq m "program"
      return (Guess id prog)
    where m = fromJSObject o
  readJSON _ = Error "Error reading Guess (not JSObject)."
  
  showJSON (Guess id prog) =
      JSObject $ toJSObject $ [
        ("id", showJSON id),
        ("program", showJSON prog)
      ]


instance JSON GuessResponse where
  
  readJSON (JSObject o) = do
      status <- lookupReq m "status"
      case status of
        "win" -> return GuessResponseWin
        "mismatch" -> do
          vals <- lookupReq m "values" >>= return . map read
          return (GuessResponseMismatch vals)
        otherwise -> do
          msg <- lookupReq m "message"
          return (GuessResponseError msg)
    where m = fromJSObject o
  readJSON _ = Error "Error reading GuessResponse (not JSObject)."
  
  showJSON GuessResponseWin =
    JSObject $ toJSObject [
      ("status", showJSON "win")
    ]
  showJSON (GuessResponseMismatch vals) =
    JSObject $ toJSObject [
      ("status", showJSON "mismatch"),
      ("values", showJSON (map toHex vals))
    ]
  showJSON (GuessResponseError msg) =
    JSObject $ toJSObject [
      ("status", showJSON "error"),
      ("values", showJSON msg)
    ]


data TrainRequest = TrainRequest (Maybe Size) (Maybe [String])
  deriving (Eq,Show)

instance JSON TrainRequest where
  
  readJSON (JSObject o) = do
      n   <- lookupOpt m "size"
      ops <- lookupOpt m "operators"
      return (TrainRequest n ops)
    where m = fromJSObject o
  readJSON _ = Error "Error reading TrainRequest (not JSObject)."
  
  showJSON (TrainRequest n ops) =
      JSObject $ toJSObject (optField "size" n ++ optField "operators" ops)


instance JSON TrainingProblem where
  
  readJSON (JSObject o) = do
      prog <- lookupReq m "challenge"
      id   <- lookupReq m "id"
      size <- lookupReq m "size"
      ops  <- lookupReq m "operators"
      return (TrainingProblem prog id size ops)
    where m = fromJSObject o
  readJSON _ = Error "Error reading TrainingProblem (not JSObject)."
  
  showJSON (TrainingProblem prog id size ops) =
    JSObject $ toJSObject [
      ("challenge", showJSON prog),
      ("id", showJSON id),
      ("size", showJSON size),
      ("operators", showJSON ops)
    ]


data RegisterRequest = RegisterRequest WorkerID
  deriving (Eq,Show)

instance JSON RegisterRequest where
  
  readJSON (JSObject o) = do
      wid <- lookupReq m "workerID"
      return (RegisterRequest wid)
    where m = fromJSObject o
  readJSON _ = Error "Error reading RegisterRequest (not JSObject)."
  
  showJSON (RegisterRequest wid) =
      JSObject $ toJSObject [("workerID", showJSON wid)]


instance JSON DistProblem where
  
  readJSON (JSObject o) = do
      pid  <- lookupReq m "id"
      wnum <- lookupReq m "workerNum"
      wtot <- lookupReq m "totalWorkers"
      return (DistProblem pid wnum wtot)
    where m = fromJSObject o
  readJSON _ = Error "Error reading DistProblem (not JSObject)."

  showJSON (DistProblem pid wnum wtot) =
      JSObject $ toJSObject [
        ("id", showJSON pid),
        ("workerNumber", showJSON wnum),
        ("totalWorkers", showJSON wtot)
      ]


instance JSON DistTrainingProblem where
  
  readJSON (JSObject o) = do
      prob <- readJSON (JSObject o)
      wnum <- lookupReq m "workerNum"
      wtot <- lookupReq m "totalWorkers"
      return (DistTrainingProblem prob wnum wtot)
    where m = fromJSObject o
  readJSON _ = Error "Error reading DistTrainingProblem (not JSObject)."

  showJSON (DistTrainingProblem prob wnum wtot) =
      JSObject $ toJSObject $
      fromJSObject probObj ++ [
        ("workerNumber", showJSON wnum),
        ("totalWorkers", showJSON wtot)
      ]
    where (JSObject probObj) = showJSON prob
  

-- ** HTTP Support Code

urlRoot = "http://icfpc2013.cloudapp.net/"
secret  = "02768XDijvjky5OOedNdAnRxokV6hSA8aaFT1doK"

-- | Send an HTTP request.
--   Clients should use `evalRequest` or `guessRequest`.
performRequest :: (JSON a, JSON b) => String -> a -> IO (Response b)
performRequest path request = do
    result <- simpleHTTP $ postRequestWithBody url "application/json" (encode request)
    case result of
      Left err -> return (ConnectionError (show err))
      Right (HTTP.Response code msg _ body) -> return $
        if code /= (2,0,0) then HTTPError code msg
        else case decode body of
               Ok a      -> OK a
               Error msg -> JSONError msg
  where url = urlRoot ++ path ++ "?auth=" ++ secret ++ "vpsH1H"


-- ** Test Code

-- demoEvalRequest1 = EvalRequest "cVBdX88Lz74jTfLTSj2YseZW" [1..256]
-- demoEvalRequest2 = EvalRequest "MFrVnSUaIMxUZ38ZDqBzwkwz" [1..256]
-- demoGuess1 = Guess "cVBdX88Lz74jTfLTSj2YseZW" "(lambda (x_1) x_1)"

{-

-- ** Unused part of JSON spec **

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
  readJSON _ = Error "Error reading Problem (not JSObject)."
  
  showJSON (Problem id size ops sol time) =
      JSObject $ toJSObject $ [
        ("id", showJSON id),
        ("size", showJSON size),
        ("operators", showJSON ops)
      ] ++ optField "solved" sol ++ optField "timeLeft" time


-- ** Unimplemented part of JSON spec **

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
