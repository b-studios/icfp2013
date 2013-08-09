module Main where

import qualified Lahnparty.Test.Language as Lang

import Test.Framework


main = defaultMain tests

tests = Lang.tests
