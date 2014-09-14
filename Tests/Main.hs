module Main where

import Appraisal.Config (Top(Top))
import Appraisal.Utils.ErrorWithIO (ErrorWithIO)
import Prelude
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import Base (testReport)
import Test1 (modifyReport)
import Test2 (test)
import LaTeX (tests)
import Test.HUnit (Test(TestList), runTestTT, Counts(errors, failures))

main =
    do counts <- runTestTT Main.tests
       putStrLn (show counts)
       case (errors counts + failures counts) of
         0 -> exitWith ExitSuccess
         _ -> exitWith (ExitFailure 1)

tests :: Test
tests =
    TestList [{-test1, Test2.test,-} LaTeX.tests]
    where
      test1 = testReport (Test1.modifyReport ver1) ver1
      ver1 = Top "testdata/test1"
