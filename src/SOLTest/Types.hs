-- | Data types for the SOLtest testing tool.
--
-- This module defines all algebraic data types used throughout the tool.
--
-- FLP: Note that the data types use a so-called “record syntax” that you maybe aren't
--      familiar with. This is merely a syntactic sugar for in-place creation of accessor
--      functions for the individual fields inside of the data value. For example, see
--      @TestCaseFile@ below. This way of defining it is exactly the same as if I wrote:
--
-- >    data TestCaseFile = TestCaseFile String FilePath (Maybe FilePath) (Maybe FilePath)
--
--      and then defined functions:
--
-- >    tcfName (TestCaseFile n _ _ _) = n             -- :: TestCaseFile -> String
-- >    tcfTestSourcePath (TestCaseFile _ p _ _) = p   -- :: TestCaseFile -> FilePath
--
--      and so on.
module SOLTest.Types
  ( -- * Test case types
    TestCaseType (..),
    TestCaseFile (..),
    TestCaseDefinition (..),
    TestName,
    TestCategory,
    TestTag,

    -- * Execution reason codes
    UnexecutedReasonCode (..),
    UnexecutedReason (..),

    -- * Test results
    TestResult (..),
    TestCaseReport (..),
    CategoryReport (..),

    -- * Report structure
    TestStats (..),
    TestReport (..),

    -- * CLI and filtering
    FilterCriterion (..),
    filterCriterionValue,
    FilterSpec (..),
    Options (..),
  )
where

import Data.Map.Strict (Map)

-- ---------------------------------------------------------------------------
-- Test case types
-- ---------------------------------------------------------------------------

type TestName = String

type TestCategory = String

type TestTag = String

-- | The type of a test case, determining how it should be executed.
data TestCaseType
  = -- | Only run the SOL26 parser.
    ParseOnly
  | -- | Only run the interpreter on pre-compiled XML.
    ExecuteOnly
  | -- | Parse SOL26 then interpret the result.
    Combined
  deriving (Eq, Show, Enum, Bounded)

-- | A discovered @.test@ file before parsing the header.
data TestCaseFile = TestCaseFile
  { -- | Test name (filename without @.test@ extension).
    tcfName :: TestName,
    -- | Absolute or relative path to the @.test@ file.
    tcfTestSourcePath :: FilePath,
    -- | Path to the @.in@ companion file, if it exists.
    tcfStdinFile :: Maybe FilePath,
    -- | Path to the @.out@ companion file, if it exists.
    tcfExpectedStdout :: Maybe FilePath
  }
  deriving (Eq, Show)

-- | A fully parsed test case definition ready for filtering and execution.
--
-- The 'tcdSourceCode' field is for internal use only and is excluded from
-- the JSON output.
data TestCaseDefinition = TestCaseDefinition
  { -- | Test name (filename without @.test@ extension).
    tcdName :: TestName,
    -- | Path to the @.test@ file.
    tcdTestSourcePath :: FilePath,
    -- | Path to the @.in@ companion file, if present.
    tcdStdinFile :: Maybe FilePath,
    -- | Path to the @.out@ companion file, if present.
    tcdExpectedStdoutFile :: Maybe FilePath,
    -- | The inferred test type.
    tcdTestType :: TestCaseType,
    -- | Optional human-readable description from the @***@ header line.
    tcdDescription :: Maybe String,
    -- | Category string from the @+++@ header line.
    tcdCategory :: TestCategory,
    -- | Tags from all @---@ header lines.
    tcdTags :: [TestTag],
    -- | Point weight from the @>>>@ header line.
    tcdPoints :: Int,
    -- | Expected parser exit codes from @!C!@ lines. 'Nothing' for 'ExecuteOnly'.
    tcdExpectedParserExitCodes :: Maybe [Int],
    -- | Expected interpreter exit codes from @!I!@ lines. 'Nothing' for 'ParseOnly'.
    tcdExpectedInterpreterExitCodes :: Maybe [Int],
    -- | The source code body (not serialized to JSON).
    tcdSourceCode :: String
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Unexecuted reason
-- ---------------------------------------------------------------------------

-- | Reason codes for why a test case was not executed.
data UnexecutedReasonCode
  = -- | Filtered out by include or exclude criteria.
    FilteredOut
  | -- | The @.test@ file could not be parsed.
    MalformedTestCase
  | -- | Test type could not be unambiguously determined.
    CannotDetermineType
  | -- | Required external executable could not be run.
    CannotExecute
  | -- | Unexpected error.
    OtherReason
  deriving (Eq, Show, Enum, Bounded)

-- | The reason a test case was not executed, with an optional message.
data UnexecutedReason = UnexecutedReason
  { -- | The reason code.
    urCode :: UnexecutedReasonCode,
    -- | An optional human-readable explanation.
    urMessage :: Maybe String
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Test results
-- ---------------------------------------------------------------------------

-- | The result of an executed test case.
data TestResult
  = -- | Test passed all checks.
    Passed
  | -- | Parser exited with an unexpected exit code.
    ParseFail
  | -- | Interpreter exited with an unexpected exit code.
    IntFail
  | -- | Interpreter output differed from expected.
    DiffFail
  deriving (Eq, Show)

-- | Report for a single executed test case.
data TestCaseReport = TestCaseReport
  { -- | The overall result of the test.
    tcrResult :: TestResult,
    -- | The parser's actual exit code, if the parser was run.
    tcrParserExitCode :: Maybe Int,
    -- | The interpreter's actual exit code, if the interpreter was run.
    tcrInterpreterExitCode :: Maybe Int,
    -- | Captured stdout from the parser.
    tcrParserStdout :: Maybe String,
    -- | Captured stderr from the parser.
    tcrParserStderr :: Maybe String,
    -- | Captured stdout from the interpreter.
    tcrInterpreterStdout :: Maybe String,
    -- | Captured stderr from the interpreter.
    tcrInterpreterStderr :: Maybe String,
    -- | Output of the diff command, if it was run.
    tcrDiffOutput :: Maybe String
  }
  deriving (Eq, Show)

-- | Report for a category of test cases.
data CategoryReport = CategoryReport
  { -- | Sum of points for all executed tests in this category.
    crTotalPoints :: Int,
    -- | Sum of points for all passed tests in this category.
    crPassedPoints :: Int,
    -- | Individual test results keyed by test name.
    crTestResults :: Map String TestCaseReport
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Statistics and top-level report
-- ---------------------------------------------------------------------------

-- | Aggregate statistics about the test run.
data TestStats = TestStats
  { -- | Total number of @.test@ files discovered on disk.
    tsFoundTestFiles :: Int,
    -- | Number of tests successfully parsed.
    tsLoadedTests :: Int,
    -- | Number of tests selected after filtering.
    tsSelectedTests :: Int,
    -- | Number of tests that passed execution.
    tsPassedTests :: Int,
    -- | Histogram of per-category pass rates. Keys are @\"0.0\"@ through @\"0.9\"@;
    -- each value is the count of categories whose pass rate falls in that bin.
    tsHistogram :: Map String Int
  }
  deriving (Eq, Show)

-- | The top-level test report written as JSON output.
data TestReport = TestReport
  { -- | All test cases that were successfully discovered and parsed.
    trDiscoveredTestCases :: [TestCaseDefinition],
    -- | Tests that were not executed, keyed by test name.
    trUnexecuted :: Map String UnexecutedReason,
    -- | Execution results grouped by category. 'Nothing' in dry-run mode;
    -- excluded from JSON when 'Nothing' or empty.
    trResults :: Maybe (Map String CategoryReport),
    -- | Aggregate statistics.
    trStats :: TestStats
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- CLI types
-- ---------------------------------------------------------------------------

-- | A single filter criterion specifying what to match against.
data FilterCriterion
  = -- | Match against test name, tag, or category (@-i@\/@-e@).
    ByAny String
  | -- | Match against category only (@--ic@\/@--ec@).
    ByCategory String
  | -- | Match against tag only (@--it@\/@--et@).
    ByTag String
  deriving (Eq, Show)

-- | Extract the string value from a 'FilterCriterion'.
filterCriterionValue :: FilterCriterion -> String
filterCriterionValue (ByAny s) = s
filterCriterionValue (ByCategory s) = s
filterCriterionValue (ByTag s) = s

-- | All filter specifications parsed from command-line arguments.
data FilterSpec = FilterSpec
  { -- | Include criteria. Empty means include all tests.
    fsIncludes :: [FilterCriterion],
    -- | Exclude criteria.
    fsExcludes :: [FilterCriterion],
    -- | When 'True', criterion values are treated as POSIX regular expressions.
    --
    -- FLP: If you're not implementing the regex matching bonus extension, you can either
    -- remove this and update the usages of the datatype throughout the codebase, or you
    -- can simply fill it with an ignored default value.
    fsUseRegex :: Bool
  }
  deriving (Eq, Show)

-- | All parsed command-line options.
data Options = Options
  { -- | Path to the directory containing test files (positional argument).
    optTestDir :: FilePath,
    -- | Path to the SOL26 parser executable (@-p@\/@--parser@).
    optParser :: Maybe FilePath,
    -- | Path to the interpreter executable (@-t@\/@--interpreter@).
    optInterpreter :: Maybe FilePath,
    -- | Search test directory recursively (@-r@\/@--recursive@).
    optRecursive :: Bool,
    -- | Write JSON report to this file instead of stdout (@-o@\/@--output@).
    optOutputFile :: Maybe FilePath,
    -- | Load and filter tests but do not execute them (@--dry-run@).
    optDryRun :: Bool,
    -- | All filtering criteria.
    optFilter :: FilterSpec
  }
  deriving (Eq, Show)
