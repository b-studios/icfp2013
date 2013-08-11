module Lahnparty.Driver where

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Word (Word64)
import System.Exit (exitFailure)


import Lahnparty.Language
import Lahnparty.WebAPI
import qualified Lahnparty.GeneratorTH  as GTH1
import qualified Lahnparty.GeneratorTH2 as GTH2
import Lahnparty.GeneratorTH2
import Lahnparty.ProblemsDB
import Lahnparty.Types

-- XXX disable in production, it changes memory usage
expensiveDebug = False

-- XXX disable in the last rush, when we should try as much as we can and
-- continue even after failures.
exitWhenFailing = True

-- | Handle a timeout, and decide whether to exit. startedAlready tells us if we
--   already started the timer on this problem with a previous request, in which
--   case a timeout is our fault and we should exit defensively. Otherwise, a
--   timeout means "this program was solved earlier".
handleTimeout :: Maybe ProblemID -> Bool -> String -> IO ()
handleTimeout pid startedAlready str = do
  putStrLn $ ">>> We failed with a timeout" ++ maybe "" (\i -> " on problem ID: " ++ show i) pid
  putStrLn $ "Error message: " ++ str
  when (startedAlready && exitWhenFailing)
    exitFailure

reportUnexpected err =
  putStrLn $ "Unknown error: " ++ show err

-- | Unexpected error, fail and exit (after creating debug output). This should
--   only be used if the timer is not currently running on a problem.
handleUnexpected err = do
  reportUnexpected err
  putStrLn "Exiting defensively."  -- unknown error
  exitFailure

-- | Wait a few seconds because of a too many requests error.
waitForRateLimit429 = do
  putStrLn "Too many requests, trying again."
  threadDelay 5000000 -- 5 seconds
    
debugShowPrograms s programs =
  when expensiveDebug $ do
    putStrLn $ "# generated programs " ++ s ++ ": " ++ show (length programs)
    putStrLn $ "First 10 generated programs:"
    mapM_ print $ take 10 programs

type EvalRequester = ProblemID -> [Word64] -> IO (Response EvalResponse)

-- | The old driver (this should work the same as before).
driver = genericDriver evalRequest

-- | Distributed driver.
distDriver wid = genericDriver (distEvalRequest wid)

genericDriver :: EvalRequester -> Generator -> ProblemID -> Size -> [Op] -> IO ()
genericDriver eval gen probId size ops = do
    
    putStrLn $ "== ProblemID: " ++ probId ++ " =="
    
    let inputs = randomInputs

    putStr "Sending eval request: "
    result <- eval probId inputs
    putStrLn "DONE!"

    case result of

      OK (EvalResponseOK newIns outputs) -> do
        
        let inputs' = maybe inputs id newIns
        let knowledge = buildKnowledge inputs' outputs
        
        putStr "Generating:"
        let programs = gen size ops knowledge
        putStrLn "(kind of) DONE!"
        debugShowPrograms "before filtering" programs
        putStr "Filtering:"
        let programsFilt = filterProgs programs inputs' outputs
        putStrLn "(kind of) DONE!"
        debugShowPrograms "after filtering" programsFilt

        getMoreInfo probId programsFilt

      -- we should only get this error code from the proxy server
      HTTPError (4,2,0) _ -> do
        putStrLn "Waiting for more workers ..."
        threadDelay 3000000 -- 3 seconds
        driver gen probId size ops

      HTTPError (4,2,9) _ -> do
        waitForRateLimit429
        driver gen probId size ops

      HTTPError (4,1,0) str -> do
        handleTimeout (Just probId) False str

      err -> do
        handleUnexpected err

    return ()

getMoreInfo probId [] = do
  putStrLn "No more possible programs"
  return ()

getMoreInfo pid (p:ps) = do
    
    putStrLn $ "Guessing program " ++ prettyP p
    res <- guessRequest pid p
    
    case res of
      
      OK GuessResponseWin -> do
        putStrLn "We won!"
        return ()
      OK (GuessResponseError str) -> do
        putStrLn $ "What? GuessResponseError " ++ str
        getMoreInfo pid ps
      OK (GuessResponseMismatch words) -> do
        putStrLn "Guess mismatch, filtering ..."
        let programsFilt = filterProgs ps [words !! 0] [words !! 1]
        when expensiveDebug $ do
          putStrLn $ "# generated programs after filtering on counterexample: " ++ show (length ps)
        getMoreInfo pid programsFilt

      -- we should only get this error code from the proxy server
      HTTPError (4,1,2) _ -> do
        putStrLn "Problem already solved by a different worker."
        return ()

      HTTPError (4,2,9) _ -> do
        waitForRateLimit429
        tryAgain

      HTTPError (4,1,0) str -> do
        handleTimeout (Just pid) True str

      err -> do
        reportUnexpected err
        putStrLn "Timer is still running, so trying again."
        tryAgain
  
  where
    tryAgain = getMoreInfo pid (p:ps)


filterProgs programs inputs outputs =
  [ program
  | program <- programs
  -- XXX use parallel list comprehensions
  , and [ evalP input program == output
        | (input, output) <- zip inputs outputs
        ]
  ]
    -- XXX speed this evaluation by SIMD evaluation (?)

randomInputs2 = [0 .. 255]

-- Jona's inputs
randomInputs =
  [ 0x1000000000000000
  , 0x0000000000000001
  , 0x0000000000000010
  , 0x0000000000000100
  , 0x0000000000001000
  , 0x0000000000010000
  , 0x0000000000100000
  , 0x0000000001000000
  , 0x0000000010000000
  , 0x0000000100000000
  , 0x0000001000000000
  , 0x0000010000000000
  , 0x0000100000000000
  , 0x0001000000000000
  , 0x0010000000000000
  , 0x0100000000000000
  , 0x1111111111111110
  , 0x1111111111111101
  , 0x1111111111111011
  , 0x1111111111110111
  , 0x1111111111101111
  , 0x1111111111011111
  , 0x1111111110111111
  , 0x1111111101111111
  , 0x1111111011111111
  , 0x1111110111111111
  , 0x1111101111111111
  , 0x1111011111111111
  , 0x1110111111111111
  , 0x1101111111111111
  , 0x1011111111111111
  , 0x0111111111111111
  , 0x0123456789ABCDEF
  , 0x123456789ABCDEF0
  , 0x23456789ABCDEF01
  , 0x3456789ABCDEF012
  , 0x456789ABCDEF0123
  , 0x56789ABCDEF01234
  , 0x6789ABCDEF012345
  , 0x789ABCDEF0123456
  , 0x89ABCDEF01234567
  , 0x9ABCDEF012345678
  , 0xABCDEF0123456789
  , 0xBCDEF0123456789A
  , 0xCDEF0123456789AB
  , 0xDEF0123456789ABC
  , 0xEF0123456789ABCD
  , 0xF0123456789ABCDE
  , 0x0102030405060708
  , 0x1020304050607080
  , 0x0203040506070801
  , 0x2030405060708010
  , 0x0304050607080102
  , 0x3040506070801020
  , 0x0405060708010203
  , 0x4050607080102030
  , 0x0506070801020304
  , 0x5060708010203040
  , 0x0607080102030405
  , 0x6070801020304050
  , 0x0708010203040506
  , 0x7080102030405060
  , 0x0801020304050607
  , 0x8010203040506070
  , 0xF1F2F3F4F5F6F7F8
  , 0x1F2F3F4F5F6F7F8F
  , 0xF2F3F4F5F6F7F8F1
  , 0x2F3F4F5F6F7F8F1F
  , 0xF3F4F5F6F7F8F1F2
  , 0x3F4F5F6F7F8F1F2F
  , 0xF4F5F6F7F8F1F2F3
  , 0x4F5F6F7F8F1F2F3F
  , 0xF5F6F7F8F1F2F3F4
  , 0x5F6F7F8F1F2F3F4F
  , 0xF6F7F8F1F2F3F4F5
  , 0x6F7F8F1F2F3F4F5F
  , 0xF7F8F1F2F3F4F5F6
  , 0x7F8F1F2F3F4F5F6F
  , 0xF8F1F2F3F4F5F6F7
  , 0x8F1F2F3F4F5F6F7F
  , 0x0000000000000000
  , 0x1111111111111111
  , 0x2222222222222222
  , 0x3333333333333333
  , 0x4444444444444444
  , 0x5555555555555555
  , 0x6666666666666666
  , 0x7777777777777777
  , 0x8888888888888888
  , 0x9999999999999999
  , 0xAAAAAAAAAAAAAAAA
  , 0xBBBBBBBBBBBBBBBB
  , 0xCCCCCCCCCCCCCCCC
  , 0xDDDDDDDDDDDDDDDD
  , 0xEEEEEEEEEEEEEEEE
  , 0xFFFFFFFFFFFFFFFF
  , 0x0102030405060708
  , 0x1112131415161718
  , 0x2122232425262728
  , 0x3132333435363738
  , 0x4142434445464748
  , 0x5152535455565758
  , 0x6162636465666768
  , 0x7172737475767778
  , 0x8182838485868788
  , 0x9192939495969798
  , 0xA1A2A3A4A5A6A7A8
  , 0xB1B2B3B4B5B6B7B8
  , 0xC1C2C3C4C5C6C7C8
  , 0xD1D2D3D4D5D6D7D8
  , 0xE1E2E3E4E5E6E7E8
  , 0xF1F2F3F4F5F6F7F8
  , 0x1020304050607080
  , 0x1121314151617181
  , 0x1222324252627282
  , 0x1323334353637383
  , 0x1424344454647484
  , 0x1525354555657585
  , 0x1626364656667686
  , 0x1727374757677787
  , 0x1828384858687888
  , 0x1929394959697989
  , 0x1A2A3A4A5A6A7A8A
  , 0x1B2B3B4B5B6B7B8B
  , 0x1C2C3C4C5C6C7C8C
  , 0x1D2D3D4D5D6D7D8D
  , 0x1E2E3E4E5E6E7E8E
  , 0x1F2F3F4F5F6F7F8F
  , 0x8DD94645D8795C1F
  , 0x7A1DE7CF9157ED7D
  , 0xD6820D026B740336
  , 0xAB4C51C9B51923C0
  , 0x533B23F62CE05BA4
  , 0x0EC41784E3321F15
  , 0xBBE8BB6203BD7DC3
  , 0x4A7615A5F3DF15E2
  , 0xF9F6E7210973321B
  , 0xA65BE864ACEABBB6
  , 0x550FAD22C650E0A3
  , 0x9B842AF6D70833D8
  , 0x8AF434CF2575D1B9
  , 0xE2FD43AFA38C9CB9
  , 0xBEE3C14C0D42902F
  , 0xD7C644A07AA69FFD
  , 0xD895CEA7CFF071A7
  , 0x437FC15551E219F2
  , 0xDBC42EA486510423
  , 0x6AB0E3556784840D
  , 0xC2079FA826EFC7E2
  , 0xA3EB4B8BDD52FED4
  , 0x046460BBC92E3289
  , 0x9633D1855F71EC03
  , 0xF6A6A6E1BCCE14CD
  , 0x9B4666B8B8C76DAF
  , 0xEFE536596F77BB4E
  , 0x093C8B8E4DF41711
  , 0xEDE59BEB32D1B298
  , 0xC436A576F7FC4613
  , 0x5EB35F945CD2396A
  , 0xE8132C6D023B7A88
  , 0x73796A9EAA62770B
  , 0xB236BC379B54B2C5
  , 0x389B7203CBACFC8F
  , 0x6F978DD8764D55D1
  , 0x795564A9C202F6ED
  , 0x5E28660B5798E154
  , 0xE01123CC83B5B39E
  , 0x8EA1F28AEED70339
  , 0x60B19067669F2744
  , 0xE306F7781279FB17
  , 0x4ABDDBC738F2F949
  , 0xF90676C73C90BE53
  , 0x3D4341B300725C6B
  , 0xE6E2F89485F943BE
  , 0x6AB347B3990D5824
  , 0xF8CC112B0B4A95C2
  , 0xE9F42BBDCA20CBE6
  , 0x923F3D3F08E646E1
  , 0x5C556CDD2A19EBD1
  , 0x93ED6DC99C49E776
  , 0x71C9A4E0C83964D0
  , 0x74D7BE56D5C60D86
  , 0xCD4F3F1216ADA66E
  , 0x9F5FC88389907D27
  , 0x667A849B9B94F081
  , 0x5A753741A62388D3
  , 0x697B5BEF59881BE3
  , 0xBE900587C77079E9
  , 0x2B68D34C46F0EF37
  , 0x2600BE49F49E1570
  , 0xA8390C0912AFF4D5
  , 0xA6E169A03E8EC929
  , 0x0AC4ADF982EC4155
  , 0x0F91C5764C3A7A91
  , 0x727B1536BFC1232B
  , 0xB3EFEC170C316FAD
  , 0x212ABC771DE218C8
  , 0xA96DF8821C85D7D5
  , 0x259FBD77763AC2A3
  , 0x8FB8B197EC8CD2C3
  , 0x96AA04DCC38751B4
  , 0x7C3C931F0DCFC616
  , 0x9A4AABD3162A5062
  , 0x18B17B1786F37A1A
  , 0xE61538FEDDBE427A
  , 0x7FE942E3341EFE6F
  , 0x20628F8B28AD6C01
  , 0x1E34296DCFAC57B3
  , 0x39833CD0A246F781
  , 0x3EBA5134660A7958
  , 0x87F84DFB0B57E5BF
  , 0x25445F25187CA245
  , 0x6C81B4A638BAB2E8
  , 0x5E55C2F290DBA788
  , 0xC0C3381605494E1E
  , 0xACB297719F090559
  , 0xBB7DAAF03F7FD952
  , 0x043843CD14BFFBA2
  , 0x7E2F2D4FAAC99FD7
  , 0x0A001CC12E8CD87D
  , 0x4A16FC71EDBCDA4B
  , 0x9B4DF9047A34CC9D
  , 0xDE80F03FC7C3C41E
  , 0x09EF1CA1D20693AB
  , 0x23A587A865D2F141
  , 0x0F61480878D5A329
  , 0xAA9B62AEA3F05DAB
  , 0xE2892EEF17E3C6AE
  , 0xCA8EDF4117A96C6A
  , 0x7B2638B5438A059D
  , 0x8CC264D8941A8304
  , 0x5A7A99ADC82FB13C
  , 0xD1891FC7DFF79808
  , 0xF0679D540519200C
  , 0x8C027B127B214622
  , 0xC27AEDF1BA34A5A5
  , 0x3A75A575D4F55F97
  , 0xAFA65571382AD4FB
  , 0xBCA84567AD5D00FA
  , 0xC9F668CF3C23A930
  , 0x85F4A16E2AF3979D
  , 0xB28EA6B95ADF6DF9
  , 0x1E80316FCBC3B54A
  , 0x772D301074950268
  , 0x3C16E99582020190
  , 0x1966FB1E8017ABAC
  , 0x084DDC24A40000A2
  , 0xE56697B69D5B7981
  , 0x24596D4D0EB88897
  , 0xAEA7A14DFDD4D640
  , 0x31E3E0D46515E742
  , 0x476C31D7586B5F42
  , 0x8A709EAF651F808E
  , 0xC69201DFBCAFF87B
  , 0xF16058DF7113893D
  , 0x386D684B9CEFA4DE
  ]


fetchTrainingData size ops = do
  resp <- trainRequestSizeOps size ops
  case resp of
    OK (TrainingProblem program id size operators) ->
      return resp
    HTTPError (4,2,9) _ -> do
      waitForRateLimit429
      fetchTrainingData size ops
    -- Timeouts should not happen here, so no timeout-specific handling.
    err -> do
      handleUnexpected err

parseTrainingData :: Response TrainingProblem -> (ProblemID, Size, [Op])
parseTrainingData (OK (TrainingProblem _ pid size ops)) = (pid, size, ops')
  where ops' = map opStringToOp $ filter (/= "bonus") ops

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

-- Copy-n-pasted top-level structure from Showtime.hs

rangeSizeStart = 16
rangeSizeEnd = 20
nProblemsForSize = 3

main = solveTrainProblemsOfSizeFromTo GTH2.findP TrainNone rangeSizeStart rangeSizeEnd

solveTrainProblemsOfSizeFromTo :: Generator -> TrainOps -> Int -> Int -> IO ()
solveTrainProblemsOfSizeFromTo g ops from to = mapM_ (solveTrainProblemsOfSize g nProblemsForSize ops) [from .. to]

solveTrainProblemsOfSize :: Generator -> Int -> TrainOps -> Size -> IO ()
solveTrainProblemsOfSize g nProbs ops size =
  sequence_ (replicate nProbs (solveATrainProblemOfSize g ops size))

solveATrainProblemOfSize :: Generator -> TrainOps -> Size -> IO ()
solveATrainProblemOfSize g ops size = do
  -- (1) First, get the raw training problem. There are different ways of doing that.

  -- (a) get a training program of the given size
  -- req <- trainRequestSize size

  -- (b) get a training program of the given size and specified ops
  resp <- fetchTrainingData size ops

  -- (c) Alternatively, to test again on some training program, modify with the response, as output by the program. Example:
  -- let resp = OK (TrainingProblem "(lambda (x_2797) (fold x_2797 0 (lambda (x_2797 x_2798) (xor x_2797 1))))" "Wa5vCJ1BYV6Hz3fv0XbKoVQs" 8 ["tfold","xor"])

  -- (2) Then, print and parse the response
  print resp

  let (probId, size, ops) = parseTrainingData resp

  -- (3) Finally, try to solve the problem
  driver g probId size ops
