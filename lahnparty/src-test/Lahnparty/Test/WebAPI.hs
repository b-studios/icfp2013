
module Lahnparty.Test.WebAPI where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertEqual)

main = defaultMain tests

tests = [testGuess]

--
-- * Test Cases
--

-- ** Testing Guess

testGuess = testGroup "TestGuess" []
-- guess is syntactically malformed. Just think of bad syntax like `(lambda (x) `
-- should return an error code (400)
  -- testCase "Bad Request" 

-- guess is on non existing challenge. DOUBLE check whether challange does not exist before
-- running this test!
-- should return error code (404)

-- guess should return "time over" for a old (real) challange
-- (Do this after we failed answering something)
-- should return error code (410)

-- guess should tell us, that a challange has been already solved
-- should return error code (412)

-- guess should raise an error if the request is too big
-- should return error code (413)

-- guess should tell "win" if successfully guessed.
-- should return error code (200) and `status` should be "win"
-- this should be implemented by querying a fresh test and then directly solve it

-- guess should provide counter example on "mismatch" (Using `(lambda (x) x)`))
-- should return error code (200), `status` should be "mismatch" and `values` should
-- be ["0x8000000000000000", "0x0000000000000080", "0x8000000000000000"]

--
-- * Test Data
--

p1ID  = "7AbvsYATqEwduM5HxvcXyja4"
p1Sol = "(lambda (x_14561) (fold x_14561 0 (lambda (x_14561 x_14562) (if0 (or (not x_14561) (shl1 0)) 1 x_14561))))"

