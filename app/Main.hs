{-# LANGUAGE ImportQualifiedPost #-}

-- | Entry point for the SOLtest testing tool.
--
-- Orchestrates the full testing pipeline:
--
-- 1. Parse command-line arguments.
-- 2. Verify the test directory exists.
-- 3. Discover @.test@ files.
-- 4. Parse each file into a 'TestCaseDefinition'.
-- 5. Filter tests by the provided include\/exclude criteria.
-- 6. Execute selected tests (unless @--dry-run@ is set).
-- 7. Assemble and output the JSON report.
module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
-- pulls in the ToJson instances

import Data.Either (rights)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import SOLTest.CLI (parseOptions)
import SOLTest.Discovery (discoverTests)
import SOLTest.Executor (executeTest)
import SOLTest.Filter (filterTests)
import SOLTest.JSON ()
import SOLTest.Parser (parseTestFile)
import SOLTest.Parser qualified
import SOLTest.Report (buildReport)
import SOLTest.Types
  ( Options
      ( optDryRun,
        optFilter,
        optInterpreter,
        optOutputFile,
        optParser,
        optRecursive,
        optTestDir
      ),
    TestCaseDefinition (tcdName, tcdTestType),
    TestCaseFile (tcfName, tcfTestSourcePath),
    TestCaseReport,
    TestCaseType (Combined, ExecuteOnly, ParseOnly),
    TestName,
    UnexecutedReason (UnexecutedReason),
    UnexecutedReasonCode
      ( CannotDetermineType,
        FilteredOut,
        MalformedTestCase
      ),
  )
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

-- | The main entry point.
main :: IO ()
main = do
  opts <- parseOptions -- parse CLI options
  run opts -- run the testing pipeline

-- | Create an IO action that represents the full testing process
-- (for the given CLI options). When executed, it will perform the necessary checks,
-- load the test files, run the tests, generate the final report and save it or print
-- it on stdout.
run :: Options -> IO ()
run opts = do
  -- 1. Check the test directory exists

  -- Extracts the "test directory" positional argument from the CLI options
  -- this is a "normal", pure function, so we use "let ... =" to just store
  -- its result in a "variable". Note that all these functions starting with
  -- "opt" below are just something that extracts an argument value from the
  -- @Options@ data container.
  let testDir = optTestDir opts

  -- @doesDirectoryExist@ is an I/O action, so we use <- here
  dirExists <- doesDirectoryExist testDir

  -- @unless@ is a monadic (applicative) function used mostly for I/O operations
  -- that don't return anything. If the first argument evaluates to False, @unless@
  -- will return the I/O action given as the second argument. Otherwise, it will
  -- return an "empty" I/O action that does exactly nothing. Note that the top-level
  -- "do" statement builds a sequence of steps: if this @unless@ evaluates to "exitWith"
  -- here, the sequence won't ever continue because the program exits as a side effect
  -- of the "exitWith" I/O action.
  unless dirExists $ do
    hPutStrLn stderr ("Error: test directory does not exist: " ++ testDir)
    exitWith (ExitFailure 1)

  -- 2. Discover .test files
  testFiles <-
    -- discoverTests is an I/O action which, when executed,
    -- returns a list of test case file "descriptors"
    discoverTests (optRecursive opts) testDir

  let foundCount = length testFiles

  -- 3. Parse each file into a TestCaseDefinition (or record failure).
  -- Here, we star building a @Map@ (dictionary, key-value tables, whatever you wanna call it)
  -- where keys are test names and values are @UnexecutedReason@s. The first possible reason
  -- for not executing a test is that it couldn't actually be parsed - the "parseUnexecuted"
  -- map will contain all such tests.
  (parsedTests, parseUnexecuted) <- parseAll testFiles

  -- 4. Filter tests
  -- This is all pure code in our library.
  let (selected, filteredOut) = filterTests (optFilter opts) parsedTests
  -- We have a list of "filtered out" test descriptors -> convert them to a map
  -- (k=test name, v=reason filtered out) to merge with the initial map with tests that
  -- we failed to parse.
  let filterUnexecuted =
        Map.fromList
          [ (tcdName t, UnexecutedReason FilteredOut Nothing)
            | t <- filteredOut
          ]
  let baseUnexecuted = Map.union parseUnexecuted filterUnexecuted

  -- 5. Execute (or skip in dry-run mode)
  (mCategoryResults, execUnexecuted) <-
    if optDryRun opts
      then return (Nothing, Map.empty)
      else do
        checkToolsProvided opts selected
        executeAll (optParser opts) (optInterpreter opts) selected

  -- Again merge with our previous map of unexecuted tests.
  let allUnexecuted = Map.union baseUnexecuted execUnexecuted

  -- 6. Build the final report
  let report = buildReport parsedTests allUnexecuted mCategoryResults selected foundCount

  -- 7. Output JSON
  let json = encode report
  case optOutputFile opts of
    -- write to stdout
    Nothing -> BL.putStr json >> putStrLn ""
    -- write to file
    Just path -> BL.writeFile path json

-- | Exit with code 2 if a required tool path was not provided.
checkToolsProvided :: Options -> [TestCaseDefinition] -> IO ()
checkToolsProvided opts selected = do
  let needsParser = any (\t -> tcdTestType t `elem` [ParseOnly, Combined]) selected
      needsInterp = any (\t -> tcdTestType t `elem` [ExecuteOnly, Combined]) selected
  -- @when@ is just like @unless@ but the condition is reversed
  when (needsParser && missingParser) $ do
    hPutStrLn stderr "Error: --parser/-p is required for the selected tests"
    exitWith (ExitFailure 2)
  when (needsInterp && missingInterp) $ do
    hPutStrLn stderr "Error: --interpreter/-t is required for the selected tests"
    exitWith (ExitFailure 2)
  where
    missingParser = isNothing $ optParser opts
    missingInterp = isNothing $ optInterpreter opts

-- | Parse all discovered test files.
--
-- Returns @(definitions, unexecuted)@ where @unexecuted@ contains entries
-- for each file that could not be parsed.
parseAll ::
  [TestCaseFile] ->
  IO ([TestCaseDefinition], Map.Map TestName UnexecutedReason)
parseAll files = do
  results <- mapM parseOne files
  let defs = rights results
      fails = [(name, reason) | Left (name, reason) <- results]
  return (defs, Map.fromList fails)

-- | Attempt to read and parse a single @.test@ file.
--
-- Returns an IO action that, when executed, returns an @Either@ value that represents
-- either a failure (tuple of test name, @UnexecutedReason@), or a successfully parsed
-- test case definition.
parseOne ::
  TestCaseFile ->
  IO (Either (TestName, UnexecutedReason) TestCaseDefinition)
parseOne tcf = do
  -- Attempts to open file for reading.
  result <- try (readFile (tcfTestSourcePath tcf)) :: IO (Either IOException String)
  case result of
    -- File opening failed -> return the "fail" tuple.
    Left err ->
      return $
        Left (tcfName tcf, UnexecutedReason MalformedTestCase (Just $ show err))
    Right contents ->
      -- File opened, @contents@ is a string with the file contents as a text (it's actually
      -- a lazy string but this doesn't matter too much). Try to parse it.
      case parseTestFile tcf contents of
        Left err ->
          let (code, message) = case err of
                (SOLTest.Parser.CannotDetermineType m) -> (CannotDetermineType, "Cannot determine test type: " ++ m)
                (SOLTest.Parser.MalformedHeader m) -> (MalformedTestCase, m)
                (SOLTest.Parser.MissingRequiredField m) -> (MalformedTestCase, "Missing required field: " ++ m)
           in return (Left (tcfName tcf, UnexecutedReason code (Just message)))
        Right def -> return (Right def)

-- | Execute all selected test cases.
--
-- The IO action yields a tuple @(Just resultsMap, unexecutedMap)@ where @resultsMap@
-- is keyed by test name for successfully executed tests, and @unexecutedMap@ holds
-- entries for tests that could not be executed.
executeAll ::
  Maybe FilePath ->
  Maybe FilePath ->
  [TestCaseDefinition] ->
  IO (Maybe (Map.Map TestName TestCaseReport), Map.Map TestName UnexecutedReason)
executeAll mParser mInterp tests = do
  pairs <- mapM (\t -> fmap (t,) (executeTest mParser mInterp t)) tests
  let results = Map.fromList [(tcdName t, r) | (t, Right r) <- pairs]
      unexecuted = Map.fromList [(tcdName t, u) | (t, Left u) <- pairs]
  return (Just results, unexecuted)
