{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'Arbitrary' instances for generating random test data.
-- Used by the QuickCheck property tests.
module SOLTest.Generators
  ( -- * Exported generators
    arbitraryCategoryReport,
    genValidHeaderLines,
    genValidTestFileContent,

    -- * Re-exported helpers
    SafeString (..),
  )
where

import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import SOLTest.Parser (ParsedHeader (..), emptyHeader)
import SOLTest.Types
    ( CategoryReport(..),
      FilterCriterion(..),
      FilterSpec(..),
      TestCaseDefinition(..),
      TestCaseFile (..),
      TestCaseReport(..),
      TestCaseType(..),
      TestResult(..) )
import Test.QuickCheck

-- ---------------------------------------------------------------------------
-- Basic string generators
-- ---------------------------------------------------------------------------

-- | Generate a non-empty printable ASCII string with no leading/trailing spaces.
newtype SafeString = SafeString {getSafeString :: String}
  deriving (Eq, Show)

instance Arbitrary SafeString where
  arbitrary = do
    -- Generate a non-empty alphanumeric string (safe as a category/tag/name)
    len <- choose (1, 20)
    chars <- vectorOf len (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['-', '_']))
    return (SafeString chars)

-- ---------------------------------------------------------------------------
-- TestCaseType
-- ---------------------------------------------------------------------------

instance Arbitrary TestCaseType where
  arbitrary = elements [ParseOnly, ExecuteOnly, Combined]

-- ---------------------------------------------------------------------------
-- ParsedHeader
-- ---------------------------------------------------------------------------

instance Arbitrary ParsedHeader where
  arbitrary = do
    desc <- arbitrary
    cat <- fmap (fmap getSafeString) arbitrary
    tags <- fmap (map getSafeString) (listOf (arbitrary :: Gen SafeString))
    weight <- arbitrary
    pCodes <- listOf (choose (0, 10))
    iCodes <- listOf (choose (0, 10))
    return
      emptyHeader
        { phDescription = fmap getSafeString desc,
          phCategory = cat,
          phTags = tags,
          phWeight = weight,
          phParserCodes = pCodes,
          phInterpreterCodes = iCodes
        }

-- ---------------------------------------------------------------------------
-- TestCaseDefinition
-- ---------------------------------------------------------------------------

-- | Generate a minimal valid 'TestCaseDefinition'.
instance Arbitrary TestCaseDefinition where
  arbitrary = do
    name <- fmap getSafeString arbitrary
    cat <- fmap getSafeString arbitrary
    tags <- fmap (map getSafeString) (listOf (arbitrary :: Gen SafeString))
    points <- choose (1, 10)
    testType <- arbitrary
    let (parserCodes, interpCodes) = codesForType testType
    return
      TestCaseDefinition
        { tcdName = name,
          tcdTestSourcePath = name ++ ".test",
          tcdStdinFile = Nothing,
          tcdExpectedStdoutFile = Nothing,
          tcdTestType = testType,
          tcdDescription = Nothing,
          tcdCategory = cat,
          tcdTags = tags,
          tcdPoints = points,
          tcdExpectedParserExitCodes = parserCodes,
          tcdExpectedInterpreterExitCodes = interpCodes,
          tcdSourceCode = ""
        }
    where
      codesForType ParseOnly = (Just [0], Nothing)
      codesForType ExecuteOnly = (Nothing, Just [0])
      codesForType Combined = (Nothing, Just [0])

-- ---------------------------------------------------------------------------
-- FilterCriterion
-- ---------------------------------------------------------------------------

instance Arbitrary FilterCriterion where
  arbitrary = do
    val <- fmap getSafeString arbitrary
    elements [ByAny val, ByCategory val, ByTag val]

-- ---------------------------------------------------------------------------
-- FilterSpec
-- ---------------------------------------------------------------------------

instance Arbitrary FilterSpec where
  arbitrary = do
    includes <- listOf arbitrary
    excludes <- listOf arbitrary
    return FilterSpec {fsIncludes = includes, fsExcludes = excludes, fsUseRegex = False}

-- ---------------------------------------------------------------------------
-- TestResult and TestCaseReport
-- ---------------------------------------------------------------------------

instance Arbitrary TestResult where
  arbitrary = elements [Passed, ParseFail, IntFail, DiffFail]

-- | Generate a 'TestCaseReport' with a random result and all optional fields
-- set to 'Nothing'.
instance Arbitrary TestCaseReport where
  arbitrary = do
    result <- arbitrary
    return
      TestCaseReport
        { tcrResult = result,
          tcrParserExitCode = Nothing,
          tcrInterpreterExitCode = Nothing,
          tcrParserStdout = Nothing,
          tcrParserStderr = Nothing,
          tcrInterpreterStdout = Nothing,
          tcrInterpreterStderr = Nothing,
          tcrDiffOutput = Nothing
        }

-- ---------------------------------------------------------------------------
-- CategoryReport
-- ---------------------------------------------------------------------------

-- | Generate a @(categoryName, 'CategoryReport')@ pair with random results.
-- Category names are drawn from a small fixed set so that collisions can occur
-- in generated lists (allowing @Map.fromList@ deduplication behaviour to be
-- exercised in tests).
arbitraryCategoryReport :: Gen (String, CategoryReport)
arbitraryCategoryReport = do
  catName <- elements ["cat_a", "cat_b", "cat_c", "cat_d"]
  reports <- listOf (arbitrary :: Gen TestCaseReport)
  let names = zipWith (\i _ -> "test_" ++ show (i :: Int)) [0 ..] reports
      totalPts = length reports
      passedPts = length [r | r <- reports, tcrResult r == Passed]
      rMap = Map.fromList (zip names reports)
  return
    ( catName,
      CategoryReport
        { crTotalPoints = totalPts,
          crPassedPoints = passedPts,
          crTestResults = rMap
        }
    )

-- ---------------------------------------------------------------------------
-- Parser generators
-- ---------------------------------------------------------------------------

-- | Generate a list of header lines that are valid for the given 'TestCaseType'.
--
-- Always includes a @+++@ category line and a @>>>@ weight line.
-- Includes @!C!@ and\/or @!I!@ lines appropriate for the type.
-- Optionally includes a @***@ description line and zero or more @---@ tag lines.
genValidHeaderLines :: TestCaseType -> Gen [String]
genValidHeaderLines testType = do
  cat <- fmap getSafeString arbitrary
  weight <- choose (1, 10 :: Int)
  desc <- arbitrary
  tags <- listOf (fmap getSafeString arbitrary)
  codes <- codesFor testType

  let descLines = maybe [] ((\d -> ["*** " ++ d]) . getSafeString) desc
      catLine = "+++ " ++ cat
      tagLines = map ("--- " ++) tags
      wgtLine = ">>> " ++ show weight
      codeLines = codes

  -- Return a fixed ordering (desc, cat, tags, weight, codes).
  -- Order-independence is tested separately; here we just need valid input.
  return (descLines ++ [catLine] ++ tagLines ++ [wgtLine] ++ codeLines)
  where
    codesFor ParseOnly = do
      cs <- listOf1 (choose (0, 5 :: Int))
      return (map (\c -> "!C! " ++ show c) cs)
    codesFor ExecuteOnly = do
      is <- listOf1 (choose (0, 5 :: Int))
      return (map (\i -> "!I! " ++ show i) is)
    codesFor Combined = do
      is <- listOf1 (choose (0, 5 :: Int))
      return ("!C! 0" : map (\i -> "!I! " ++ show i) is)

-- | Generate a complete @.test@ file content string and a matching
-- 'TestCaseFile' for a given 'TestCaseType'.
--
-- Returns @(fileContent, tcf)@ where @fileContent@ is what would be read
-- from the @.test@ file on disk.
genValidTestFileContent :: TestCaseType -> Gen (String, TestCaseFile)
genValidTestFileContent testType = do
  hdrLines <- genValidHeaderLines testType
  bodyLines <- listOf (fmap getSafeString arbitrary)
  name <- fmap getSafeString arbitrary

  let body = intercalate "\n" bodyLines
      content = unlines hdrLines ++ "\n" ++ body
      tcf =
        TestCaseFile
          { tcfName = name,
            tcfTestSourcePath = name ++ ".test",
            tcfStdinFile = Nothing,
            tcfExpectedStdout = Nothing
          }
  return (content, tcf)
