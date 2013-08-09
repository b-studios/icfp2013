module Main where

import qualified Lahnparty.Test.WebAPI as WebAPI

import Test.Framework


main = defaultMain tests

tests = WebAPI.tests
