module Main where

import Test.Runner

import RestfulRouting

main = testRunnerMain [("gets index", TestRunnerTest testGetsIndex)]
