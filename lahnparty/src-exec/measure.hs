module Main where

import Lahnparty.GeneratorTH2
import Lahnparty.Language

import Control.Monad(when)
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

programs size = findP size nofoldops undefined
                ++ findP size (OpFold:nofoldops) undefined

main = do
         args <- getArgs
         when (null args) $ print "usage: program size"
         let size = read $ head args
         print $ length $ programs size