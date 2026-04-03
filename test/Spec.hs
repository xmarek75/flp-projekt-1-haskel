-- | Main test runner for the SOLtest tool's QuickCheck property tests.
module Main (main) where

import SOLTest.FilterSpec (filterProps)
import SOLTest.ParserSpec (parserProps)
import SOLTest.ReportSpec (reportProps)

-- | Run all property test suites.
main :: IO ()
main = do
  parserProps
  filterProps
  reportProps
