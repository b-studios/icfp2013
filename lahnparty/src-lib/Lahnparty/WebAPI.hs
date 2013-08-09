{-# LANGUAGE PatternGuards, DoAndIfThenElse #-}

module Lahnparty.WebAPI where

import Numeric

import Data.Word (Word64)
import Text.JSON
import Network.HTTP (ResponseCode,postRequestWithBody,simpleHTTP)
import qualified Network.HTTP as HTTP

import Lahnparty.Language


--
-- * Public Interface
--

type ProblemID = String

-- | Generic response to a request. Separates the various kinds of errors.
data Response a =
    OK a
  | ConnectionError String
  | HTTPError ResponseCode String
  | JSONError String
  deriving (Eq,Show)


-- ** Submitting Evaluation Requests

-- | Send an evaluation request.
--   Note that there are two kinds of eval requests:
--     1. request the results of solution for up to 256 arguments
--     2. compute the results of a program for up to 256 arguments
--   We implement only the first since we can compute the second on our own.
evalRequest :: ProblemID -> [Word64] -> IO (Response EvalResponse)
evalRequest id vs = performRequest "eval" (EvalRequest id vs)

data EvalResponse = 
    EvalResponseOK [Word64]
  | EvalResponseError String
  deriving (Eq,Show)


-- ** Submitting Guesses

-- | Send a guess request.
guessRequest :: ProblemID -> P -> IO (Response GuessResponse)
guessRequest id p = performRequest "guess" (Guess id (prettyP p))

-- | Send a guess request with a program represented as a string.
guessRequestString :: ProblemID -> String -> IO (Response GuessResponse)
guessRequestString id s = performRequest "guess" (Guess id s)

data GuessResponse =
    GuessResponseWin
  | GuessResponseMismatch [Word64]
  | GuessResponseError String
  deriving (Eq,Show)


-- ** Submitting a Training Request

-- | Send a request for an arbitrary training problem.
trainRequest :: IO (Response TrainingProblem)
trainRequest = performRequest "train" (TrainRequest Nothing Nothing)

-- | Send a request for a training problem of a specified size.
trainRequestSize :: Int -> IO (Response TrainingProblem)
trainRequestSize n = performRequest "train" (TrainRequest (Just n) Nothing)

-- | Send a request for a training problem of a specified size
--   with specified ops.
trainRequestSizeOps :: Int -> [Op1] -> [Op2] -> IO (Response TrainingProblem)
trainRequestSizeOps n op1s op2s =
    performRequest "train" (TrainRequest (Just n) (Just ops))
  where ops = map show op1s ++ map show op2s

-- TODO: Parse program string and op lists?
data TrainingProblem = TrainingProblem String ProblemID Int [String]
  deriving (Eq,Show)


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

data EvalRequest = EvalRequest ProblemID [Word64]
  deriving (Eq,Show)

instance JSON EvalRequest where
  
  readJSON (JSObject o) = do
      id   <- lookupReq m "id"
      args <- lookupReq m "arguments" >>= return . map read
      return (EvalRequest id args)
    where m = fromJSObject o
  readJSON _ = Error "Error reading EvalRequest (not JSObject)."

  showJSON (EvalRequest id args) =
      JSObject $ toJSObject [
        ("id", showJSON id),
        ("arguments", showJSON (map toHex args))
      ]
    
instance JSON EvalResponse where
  
  readJSON (JSObject o) = do
      status <- lookupReq m "status"
      if status == "ok" then do
        outs <- lookupReq m "outputs" >>= return . map read
        return (EvalResponseOK outs)
      else do
        msg <- lookupReq m "message"
        return (EvalResponseError msg)
    where m = fromJSObject o
  readJSON _ = Error "Error reading EvalResponse (not JSObject)."

  showJSON (EvalResponseOK outs) =
    JSObject $ toJSObject [
      ("status", showJSON "ok"),
      ("outputs", showJSON (map toHex outs))
    ]
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


data TrainRequest = TrainRequest (Maybe Int) (Maybe [String])
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

demoEvalRequest1 = EvalRequest "cVBdX88Lz74jTfLTSj2YseZW" [1..256]
demoEvalRequest2 = EvalRequest "MFrVnSUaIMxUZ38ZDqBzwkwz" [1..256]

demoGuess1 = Guess "cVBdX88Lz74jTfLTSj2YseZW" "(lambda (x_1) x_1)"


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
