{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | JSON serialization instances for all SOLtest report types.
module SOLTest.JSON () where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict qualified as Map
import SOLTest.Types

-- ---------------------------------------------------------------------------
-- TestCaseType
-- ---------------------------------------------------------------------------

-- | Serializes as an integer: 0 ('ParseOnly'), 1 ('ExecuteOnly'), 2 ('Combined').
instance ToJSON TestCaseType where
  toJSON = toJSON . testCaseTypeInt
    where
      testCaseTypeInt :: TestCaseType -> Int
      testCaseTypeInt ParseOnly = 0
      testCaseTypeInt ExecuteOnly = 1
      testCaseTypeInt Combined = 2

-- ---------------------------------------------------------------------------
-- TestCaseDefinition
-- ---------------------------------------------------------------------------

-- | Serializes all fields except 'tcdSourceCode'.
-- 'Maybe' fields serialize as JSON @null@ when 'Nothing'.
instance ToJSON TestCaseDefinition where
  toJSON tc =
    object
      [ "name" .= tcdName tc,
        "test_source_path" .= tcdTestSourcePath tc,
        "stdin_file" .= tcdStdinFile tc,
        "expected_stdout_file" .= tcdExpectedStdoutFile tc,
        "test_type" .= tcdTestType tc,
        "description" .= tcdDescription tc,
        "category" .= tcdCategory tc,
        "tags" .= tcdTags tc,
        "points" .= tcdPoints tc,
        "expected_parser_exit_codes" .= tcdExpectedParserExitCodes tc,
        "expected_interpreter_exit_codes" .= tcdExpectedInterpreterExitCodes tc
      ]

-- ---------------------------------------------------------------------------
-- UnexecutedReasonCode and UnexecutedReason
-- ---------------------------------------------------------------------------

-- | Serializes as an integer 0–4.
instance ToJSON UnexecutedReasonCode where
  toJSON = toJSON . unexecutedReasonCodeInt
    where
      unexecutedReasonCodeInt :: UnexecutedReasonCode -> Int
      unexecutedReasonCodeInt FilteredOut = 0
      unexecutedReasonCodeInt MalformedTestCase = 1
      unexecutedReasonCodeInt CannotDetermineType = 2
      unexecutedReasonCodeInt CannotExecute = 3
      unexecutedReasonCodeInt OtherReason = 4

-- | Serializes as @{\"code\": \<int\>, \"message\": \<string|null\>}@.
instance ToJSON UnexecutedReason where
  toJSON ur =
    object
      [ "code" .= urCode ur,
        "message" .= urMessage ur
      ]

-- ---------------------------------------------------------------------------
-- TestResult
-- ---------------------------------------------------------------------------

-- | Serializes as a string: @\"passed\"@, @\"parse_fail\"@, @\"int_fail\"@,
-- or @\"diff_fail\"@.
instance ToJSON TestResult where
  toJSON = toJSON . testResultString
    where
      testResultString :: TestResult -> String
      testResultString Passed = "passed"
      testResultString ParseFail = "parse_fail"
      testResultString IntFail = "int_fail"
      testResultString DiffFail = "diff_fail"

-- ---------------------------------------------------------------------------
-- TestCaseReport
-- ---------------------------------------------------------------------------

-- | All fields are included; 'Nothing' values serialize as JSON @null@.
instance ToJSON TestCaseReport where
  toJSON tcr =
    object
      [ "result" .= tcrResult tcr,
        "parser_exit_code" .= tcrParserExitCode tcr,
        "interpreter_exit_code" .= tcrInterpreterExitCode tcr,
        "parser_stdout" .= tcrParserStdout tcr,
        "parser_stderr" .= tcrParserStderr tcr,
        "interpreter_stdout" .= tcrInterpreterStdout tcr,
        "interpreter_stderr" .= tcrInterpreterStderr tcr,
        "diff_output" .= tcrDiffOutput tcr
      ]

-- ---------------------------------------------------------------------------
-- CategoryReport
-- ---------------------------------------------------------------------------

instance ToJSON CategoryReport where
  toJSON cr =
    object
      [ "total_points" .= crTotalPoints cr,
        "passed_points" .= crPassedPoints cr,
        "test_results" .= crTestResults cr
      ]

-- ---------------------------------------------------------------------------
-- TestStats
-- ---------------------------------------------------------------------------

instance ToJSON TestStats where
  toJSON ts =
    object
      [ "total_found_test_files" .= tsFoundTestFiles ts,
        "total_loaded_tests" .= tsLoadedTests ts,
        "total_selected_tests" .= tsSelectedTests ts,
        "total_passed_tests" .= tsPassedTests ts,
        "category_success_rate_histogram" .= tsHistogram ts
      ]

-- ---------------------------------------------------------------------------
-- TestReport
-- ---------------------------------------------------------------------------

-- | The @results@ key is excluded entirely when 'trResults' is 'Nothing' or
-- an empty map.
instance ToJSON TestReport where
  toJSON tr =
    object $
      [ "discovered_test_cases" .= trDiscoveredTestCases tr,
        "unexecuted" .= trUnexecuted tr,
        "stats" .= trStats tr
      ]
        ++ resultsField
    where
      resultsField = case trResults tr of
        Nothing -> []
        Just m
          | Map.null m -> []
          | otherwise -> ["results" .= m]
