-- | Parsing the SOLtest @.test@ file format.
module SOLTest.Parser
  ( -- * Entry point
    parseTestFile,
    ParseError (..),

    -- * Intermediate types and functions (exposed for testing)
    ParsedHeader (..),
    emptyHeader,
    splitHeaderBody,
    parseHeader,
    parseHeaderLine,
    determineTestType,
  )
where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import SOLTest.Types
  ( TestCaseDefinition (..),
    TestCaseFile
      ( tcfExpectedStdout,
        tcfName,
        tcfStdinFile,
        tcfTestSourcePath
      ),
    TestCaseType (..),
  )

-- ---------------------------------------------------------------------------
-- Intermediate header type
-- ---------------------------------------------------------------------------

-- | Accumulator for the values parsed from a SOLtest header.
data ParsedHeader = ParsedHeader
  { -- | Value from the @***@ line.
    phDescription :: Maybe String,
    -- | Value from the @+++@ line.
    phCategory :: Maybe String,
    -- | Values from all @---@ lines (in order).
    phTags :: [String],
    -- | Value from the @>>>@ line.
    phWeight :: Maybe Int,
    -- | Values from all @!C!@ lines.
    phParserCodes :: [Int],
    -- | Values from all @!I!@ lines.
    phInterpreterCodes :: [Int]
  }
  deriving (Eq, Show)

data ParseError = MalformedHeader String | MissingRequiredField String | CannotDetermineType String
  deriving (Eq, Show)

-- | An empty header with no fields set.
emptyHeader :: ParsedHeader
emptyHeader =
  ParsedHeader
    { phDescription = Nothing,
      phCategory = Nothing,
      phTags = [],
      phWeight = Nothing,
      phParserCodes = [],
      phInterpreterCodes = []
    }

-- ---------------------------------------------------------------------------
-- File splitting
-- ---------------------------------------------------------------------------

-- | Split the contents of a @.test@ file into header lines and body.
--
-- The split point is the __first__ empty line (a line containing only
-- whitespace). Lines before that point are header lines; everything after
-- is the body (source code), joined back together with newlines.
--
-- If there is no empty line, all lines are treated as header lines and the
-- body is empty.
--
-- FLP: Implement this function.
splitHeaderBody :: String -> ([String], String)
splitHeaderBody content = undefined

-- ---------------------------------------------------------------------------
-- Header line parsing
-- ---------------------------------------------------------------------------

-- | Parse a single header line, updating the accumulated 'ParsedHeader'.
--
-- Returns 'Left' with an error message if the line has a known prefix but
-- a malformed value (e.g. a non-integer weight). Lines with unrecognised
-- prefixes are silently ignored, as the spec does not prohibit extra lines.
--
-- FLP: Implement the rules for all accepted headers.
parseHeaderLine :: ParsedHeader -> String -> Either String ParsedHeader
parseHeaderLine hdr line
  | "*** " `isPrefixOf` line =
      let val = trim (drop 4 line)
       in Right hdr {phDescription = Just val}
  -- ???
  | otherwise = Right hdr -- unknown or comment line: skip

-- | Parse all header lines into a 'ParsedHeader'.
--
-- Processes each line in order using 'parseHeaderLine'. Stops and returns
-- 'Left' on the first error.
parseHeader :: [String] -> Either ParseError ParsedHeader
parseHeader = foldl step (Right emptyHeader)
  where
    step (Left err) _ = Left err
    step (Right hdr) line = case parseHeaderLine hdr line of
      Left msg -> Left $ MalformedHeader msg
      Right x -> Right x

-- ---------------------------------------------------------------------------
-- Test type inference
-- ---------------------------------------------------------------------------

-- | Infer the 'TestCaseType' from a 'ParsedHeader'.
--
-- Rules:
--
-- * Has @!C!@ codes and __no__ @!I!@ codes → 'ParseOnly'
-- * Has @!I!@ codes and __no__ @!C!@ codes → 'ExecuteOnly'
-- * Has @!I!@ codes and @!C!@ is either absent or exactly @[0]@ → 'Combined'
-- * Otherwise → 'Left' (cannot determine type)
determineTestType :: ParsedHeader -> Either ParseError TestCaseType
determineTestType hdr =
  case (phParserCodes hdr, phInterpreterCodes hdr) of
    (_ : _, []) ->
      -- Parser codes present, no interpreter codes → PARSE_ONLY
      Right ParseOnly
    ([], _ : _) ->
      -- No parser codes, interpreter codes present → EXECUTE_ONLY
      Right ExecuteOnly
    (cs, _ : _)
      | null cs || cs == [0] ->
          -- Interpreter codes present, parser codes absent or exactly [0] → COMBINED
          Right Combined
      | otherwise ->
          Left $ CannotDetermineType "invalid combination of !C! and !I! codes"
    ([], []) ->
      Left $ CannotDetermineType "no !C! or !I! codes specified"

-- ---------------------------------------------------------------------------
-- Full file parsing
-- ---------------------------------------------------------------------------

-- | Parse the contents of a @.test@ file into a 'TestCaseDefinition'.
--
-- Returns 'Left' with a @ParseError@ value if:
--
-- * The header is malformed (bad exit code value, etc.)
-- * Required fields (@+++@ category, @>>>@ weight) are missing
-- * The test type cannot be determined from the exit code declarations
parseTestFile :: TestCaseFile -> String -> Either ParseError TestCaseDefinition
parseTestFile tcf content = do
  let (hdrLines, body) = splitHeaderBody content
  hdr <- parseHeader hdrLines

  -- Validate required fields
  category <- maybe (Left $ MissingRequiredField "+++ (category)") Right (phCategory hdr)
  weight <- maybe (Left $ MissingRequiredField ">>> (points)") Right (phWeight hdr)

  testType <- determineTestType hdr

  -- Build the exit code fields according to the inferred type
  let (parserCodes, interpCodes) = buildExitCodes testType hdr

  return
    TestCaseDefinition
      { tcdName = tcfName tcf,
        tcdTestSourcePath = tcfTestSourcePath tcf,
        tcdStdinFile = tcfStdinFile tcf,
        tcdExpectedStdoutFile = tcfExpectedStdout tcf,
        tcdTestType = testType,
        tcdDescription = phDescription hdr,
        tcdCategory = category,
        tcdTags = phTags hdr,
        tcdPoints = weight,
        tcdExpectedParserExitCodes = parserCodes,
        tcdExpectedInterpreterExitCodes = interpCodes,
        tcdSourceCode = body
      }

-- | Build the expected exit code lists from the parsed header and inferred type.
--
-- For 'Combined' tests: if no @!C!@ codes were given, 'tcdExpectedParserExitCodes'
-- is 'Nothing' (the parser must exit 0, which is implicit and not stored in the
-- list); if @!C! 0@ was explicit, it is stored as @Just [0]@.
--
-- FLP: Implement this function.
buildExitCodes :: TestCaseType -> ParsedHeader -> (Maybe [Int], Maybe [Int])
buildExitCodes = undefined

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Remove leading and trailing whitespace from a string.
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
