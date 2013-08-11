module Main where

import Lahnparty.GeneratorTH2
import Lahnparty.Language

import Control.Monad(when)
import qualified Data.Vector as V
import System.Environment(getArgs)

nofoldops = [OpOp1 Not, 
             OpOp1 Shl1,
             OpOp1 Shr1,
             OpOp1 Shr4,
             OpOp1 Shr16,
             OpOp2 And,
             OpOp2 Or,
             OpOp2 Xor,
             OpOp2 Plus,
             OpIf0]

knowledge = V.fromList [know 0 1, know 1 0]

programs size = findP size nofoldops knowledge
                ++ findP size (OpFold:nofoldops) knowledge

main = do
         args <- getArgs
         when (null args) $ print "usage: program size"
         let size = read $ head args
         print $ length $ programs size