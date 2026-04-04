-- | Executing test cases by running external parser and interpreter processes.
--
-- Each test case is executed according to its 'TestCaseType':
--
-- * 'ParseOnly': run the parser with source on stdin, check exit code.
-- * 'ExecuteOnly': write XML to a temp file, run the interpreter, check
--   exit code, optionally diff stdout against @.out@.
-- * 'Combined': run the parser first (must exit 0), write its output to a
--   temp file, then run the interpreter as in 'ExecuteOnly'.
module SOLTest.Executor
  ( executeTest,
    runParser,
    runInterpreter,
    runDiff,
  )
where

import Control.Exception (IOException, try)
import Data.Maybe (fromMaybe)
import SOLTest.Types
import System.Exit (ExitCode (..))
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)
import System.Process (proc, readCreateProcessWithExitCode)
import System.Directory (doesFileExist, getPermissions, executable)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Execute a single test case and return a @TestCaseReport@.
--
-- Returns @Left UnexecutedReason@ when execution cannot proceed (e.g.
-- the required executable is missing or not executable).
executeTest ::
  -- | Path to the parser executable (required for 'ParseOnly' and 'Combined').
  Maybe FilePath ->
  -- | Path to the interpreter executable (required for 'ExecuteOnly' and 'Combined').
  Maybe FilePath ->
  TestCaseDefinition ->
  IO (Either UnexecutedReason TestCaseReport)
executeTest mParser mInterp test =
  case tcdTestType test of
    ParseOnly ->
      withExecutable mParser $ \parserPath ->
        Right <$> executeParseOnly parserPath test
    ExecuteOnly ->
      withExecutable mInterp $ \interpPath ->
        Right <$> executeExecuteOnly interpPath test
    Combined ->
      withExecutable mParser $ \parserPath ->
        withExecutable mInterp $ \interpPath ->
          Right <$> executeCombined parserPath interpPath test

-- ---------------------------------------------------------------------------
-- Per-type execution
-- ---------------------------------------------------------------------------

-- | Execute a 'ParseOnly' test case.
executeParseOnly :: FilePath -> TestCaseDefinition -> IO TestCaseReport
executeParseOnly parserPath test = do
  (exitCode, pOut, pErr) <- runParser parserPath (tcdSourceCode test)
  let code = exitCodeToInt exitCode
      result
        | code `elem` expectedCodes = Passed
        | otherwise = ParseFail
      expectedCodes = fromMaybe [] (tcdExpectedParserExitCodes test)
  return
    TestCaseReport
      { tcrResult = result,
        tcrParserExitCode = Just code,
        tcrInterpreterExitCode = Nothing,
        tcrParserStdout = Just pOut,
        tcrParserStderr = Just pErr,
        tcrInterpreterStdout = Nothing,
        tcrInterpreterStderr = Nothing,
        tcrDiffOutput = Nothing
      }

-- | Execute an 'ExecuteOnly' test case.
executeExecuteOnly :: FilePath -> TestCaseDefinition -> IO TestCaseReport
executeExecuteOnly interpPath test =
  withTempSource (tcdSourceCode test) $ \tmpPath -> do
    (exitCode, iOut, iErr) <- runInterpreter interpPath tmpPath (tcdStdinFile test)
    let code = exitCodeToInt exitCode
        expectedCodes = fromMaybe [] (tcdExpectedInterpreterExitCodes test)
    (result, diffOut) <- checkInterpreterResult code expectedCodes iOut (tcdExpectedStdoutFile test)
    return
      TestCaseReport
        { tcrResult = result,
          tcrParserExitCode = Nothing,
          tcrInterpreterExitCode = Just code,
          tcrParserStdout = Nothing,
          tcrParserStderr = Nothing,
          tcrInterpreterStdout = Just iOut,
          tcrInterpreterStderr = Just iErr,
          tcrDiffOutput = diffOut
        }

-- | Execute a 'Combined' test case.
--
-- FLP: Implement this function. You'll use @withTempSource@ here.
executeCombined :: FilePath -> FilePath -> TestCaseDefinition -> IO TestCaseReport
executeCombined parserPath interpPath test = do
  (exitCode, pOut, pErr) <- runParser parserPath (tcdSourceCode test)
  let pcode = exitCodeToInt exitCode
  if pcode == 0
    then withTempSource pOut $ \tmpPath -> do
      (interpExitCode, iOut, iErr) <- runInterpreter interpPath tmpPath (tcdStdinFile test)
      let icode = exitCodeToInt interpExitCode
          expectedCodes = fromMaybe [] (tcdExpectedInterpreterExitCodes test)
      (result, diffOut) <- checkInterpreterResult icode expectedCodes iOut (tcdExpectedStdoutFile test)
      return TestCaseReport
        { tcrResult = result,
          tcrParserExitCode = Just pcode,
          tcrInterpreterExitCode = Just icode,
          tcrParserStdout = Just pOut,
          tcrParserStderr = Just pErr,
          tcrInterpreterStdout = Just iOut,
          tcrInterpreterStderr = Just iErr,
          tcrDiffOutput = diffOut
        }
    else
      return TestCaseReport
        { tcrResult = ParseFail,
          tcrParserExitCode = Just pcode,
          tcrInterpreterExitCode = Nothing,
          tcrParserStdout = Just pOut,
          tcrParserStderr = Just pErr,
          tcrInterpreterStdout = Nothing,
          tcrInterpreterStderr = Nothing,
          tcrDiffOutput = Nothing
        } 
  

-- ---------------------------------------------------------------------------
-- Process wrappers
-- ---------------------------------------------------------------------------

-- | Run the SOL26 parser by feeding @sourceCode@ on its stdin.
--
-- Returns @(exitCode, stdout, stderr)@.
runParser :: FilePath -> String -> IO (ExitCode, String, String)
runParser parserPath = readCreateProcessWithExitCode (proc parserPath [])

-- | Run the interpreter with @--source \<xmlFile\>@ and, optionally,
-- @--input \<stdinFile\>@.
--
-- Returns @(exitCode, stdout, stderr)@.
runInterpreter ::
  FilePath ->
  FilePath ->
  Maybe FilePath ->
  IO (ExitCode, String, String)
runInterpreter interpPath xmlFile mInputFile = do
  let args = ["--source", xmlFile] ++ maybe [] (\f -> ["--input", f]) mInputFile
  readCreateProcessWithExitCode (proc interpPath args) ""

-- | Run GNU @diff@ between two files (no additional flags).
--
-- Returns @(exitCode, diffOutput)@. Exit code 0 means no differences;
-- exit code 1 means differences were found.
runDiff :: FilePath -> FilePath -> IO (ExitCode, String)
runDiff actualFile expectedFile = do
  (exitCode, out, _) <- readCreateProcessWithExitCode (proc "diff" [actualFile, expectedFile]) ""
  return (exitCode, out)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Check the interpreter's result and optionally run diff.
--
-- Runs diff only when the interpreter exited with code 0 AND a @.out@ file
-- is present.
--
-- FLP: Implement this function.
checkInterpreterResult ::
  -- | Actual interpreter exit code.
  Int -> 
  -- | Expected interpreter exit codes.
  [Int] ->
  -- | Interpreter stdout.
  String ->
  -- | Path to the @.out@ file, if present.
  Maybe FilePath ->
  IO (TestResult, Maybe String)
checkInterpreterResult actualCode expectedCodes iOut mOutFile = 
  if actualCode `elem` expectedCodes then
      if actualCode == 0 then
        case mOutFile of
          Just outFile -> do
            outFileExists <- doesFileExist outFile --kontrola existujícího .out souboru
            if outFileExists then do
              (diffexitcode, diffOutput) <- runDiffOnOutput iOut outFile --pokud .out existuje, spustí diff a vrátí jeho výsledek
              return (diffexitcode, diffOutput)
            else return (Passed, Just ("Expected output file not found: " ++ outFile))
          Nothing -> return (Passed, Nothing)
      else return (Passed, Nothing)
  else return (IntFail, Nothing)


  

-- | Write a string to a temporary file and pass its path to an action.
-- The file is deleted when the action returns.
withTempSource :: String -> (FilePath -> IO a) -> IO a
withTempSource content action =
  withSystemTempFile "sol-source.xml" $ \tmpPath tmpHandle -> do
    hPutStr tmpHandle content
    hClose tmpHandle
    action tmpPath

-- | Write the interpreter stdout to a temp file and diff it against @.out@.
-- The file is deleted when the action returns.
--
-- FLP: Implement this function. It will start similarly to @withTempSource@.
runDiffOnOutput :: String -> FilePath -> IO (TestResult, Maybe String)
runDiffOnOutput iOut outFile = 
  withSystemTempFile "interpreter-output.xml" $ \tmpPath tmpHandle -> do
    hPutStr tmpHandle iOut
    hClose tmpHandle
    (diffCode, diffOut) <- runDiff tmpPath outFile
    -- Určení výsledku na základě diff exit code
    let diffPassed = diffCode == ExitSuccess
    let result =
          if diffPassed
            then Passed
            else DiffFail
    -- Vrátíme diff výstup jen pokud se output liší
    let diffMessage =
          if diffPassed
            then Nothing
            else Just diffOut
    
    return (result, diffMessage)

-- | Ensure an executable path is provided and the file is executable,
-- then run an action with it.  Returns 'Left' 'CannotExecute' if the
-- path is missing or the file is not executable.
withExecutable ::
  Maybe FilePath ->
  (FilePath -> IO (Either UnexecutedReason TestCaseReport)) ->
  IO (Either UnexecutedReason TestCaseReport)
withExecutable Nothing _ =
  return
    ( Left
        UnexecutedReason
          { urCode = CannotExecute,
            urMessage = Just "Required executable path was not provided"
          }
    )
withExecutable (Just path) action = do
  check <- checkExecutable path
  case check of
    Just reason -> return (Left reason)
    Nothing -> action path

-- | Check that a file exists and has its executable bit set.
-- The IO action returns 'Nothing' if the file is usable, or 'Just'
-- an 'UnexecutedReason' describing the problem.
--
-- FLP: Implement this function. The following functions may come in handy:
--      @doesFileExist@, @getPermissions@, @executable@
checkExecutable :: FilePath -> IO (Maybe UnexecutedReason)
checkExecutable path = do
  result <- try (doesFileExist path) :: IO (Either IOException Bool)
  case result of
    Left err -> return (Just (UnexecutedReason CannotExecute (Just (show err))))
    Right False -> return (Just (UnexecutedReason CannotExecute (Just "File does not exist")))
    Right True -> do
      permResult <- getPermissions path
      if executable permResult
        then return Nothing
        else return (Just (UnexecutedReason CannotExecute (Just "File is not executable")))

-- | Convert 'ExitCode' to an 'Int'.
exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess = 0
exitCodeToInt (ExitFailure n) = n

