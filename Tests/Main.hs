module Main where

import Appraisal.Config (Top(Top))
import Base (testReport)
import LaTeX (tests)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import Test.HUnit (Test(TestList), runTestTT, Counts(errors, failures))
import Test1 (modifyReport)
import Test2 (test)

main :: IO ()
main =
    do putStrLn "Note: you must be root, there must be a database in /srv/appraisalscribe-development, and the server must not be running."
       counts <- runTestTT Main.tests
       putStrLn (show counts)
       case (errors counts + failures counts) of
         0 -> exitWith ExitSuccess
         _ -> do 
           exitWith (ExitFailure 1)

tests :: Test
tests =
    TestList [LaTeX.tests, test1, Test2.test]
    where
      test1 = testReport (Test1.modifyReport ver1) ver1
      ver1 = Top "testdata/test1"
