-- | Comprehensive QuickCheck property tests for 'SOLTest.Parser'.
module SOLTest.ParserSpec
  ( parserProps,
  )
where

import Data.Maybe (isJust, isNothing)
import SOLTest.Generators (SafeString (..), genValidTestFileContent)
import SOLTest.Parser
    ( determineTestType,
      emptyHeader,
      parseHeaderLine,
      splitHeaderBody,
      ParsedHeader(phParserCodes, phDescription, phCategory, phTags,
                   phWeight, phInterpreterCodes) )
import SOLTest.Types
    ( TestCaseType(Combined, ParseOnly, ExecuteOnly) )
import Test.QuickCheck

-- ===========================================================================
-- splitHeaderBody
-- ===========================================================================

-- | 'splitHeaderBody' preserves all header lines before the first blank line.
prop_splitPreservesHeader :: Property
prop_splitPreservesHeader =
  forAll headerLines $ \hdr ->
    forAll bodyLines $ \bodyLs ->
      let content = unlines hdr ++ "\n" ++ unlines bodyLs
          (hdrResult, _) = splitHeaderBody content
       in hdrResult == hdr
  where
    headerLines = listOf (elements ["*** desc", "+++ cat", "--- tag", ">>> 1", "!C! 0"])
    bodyLines = listOf (elements ["line 1", "line 2", "if x then y"])

-- | When there is no blank line, the body is empty.
prop_noBlankLineEmptyBody :: Property
prop_noBlankLineEmptyBody =
  forAll safeHeaderLines $ \ls ->
    let content = unlines ls
        (_, body) = splitHeaderBody content
     in body == ""
  where
    safeHeaderLines = listOf (elements ["*** d", "+++ c", "--- t", ">>> 1", "!C! 0", "!I! 0", "abc"])

-- | The header and body lines together round-trip through 'splitHeaderBody'.
prop_splitPreservesContent :: Property
prop_splitPreservesContent =
  forAll (listOf (elements ["*** d", "+++ c", "--- t", ">>> 1"])) $ \hdr ->
    forAll (listOf (elements ["x = 1", "y = 2"])) $ \bodyLs ->
      let content = unlines hdr ++ "\n" ++ unlines bodyLs
          (hdrResult, bodyResult) = splitHeaderBody content
       in hdrResult == hdr && lines bodyResult == bodyLs

-- | Body lines that themselves contain blank lines are preserved correctly.
prop_splitBodyPreservesContent :: Property
prop_splitBodyPreservesContent =
  forAll (listOf (elements ["*** d", "+++ c"])) $ \hdr ->
    forAll (listOf (elements ["x = 1", "", "y = 2", "z = 3"])) $ \bodyLs ->
      let content = unlines hdr ++ "\n" ++ unlines bodyLs
          (_, bodyResult) = splitHeaderBody content
       in lines bodyResult == bodyLs

-- | Empty input gives empty header and empty body.
prop_splitEmptyInput :: Bool
prop_splitEmptyInput =
  splitHeaderBody "" == ([], "")

-- ===========================================================================
-- parseHeaderLine — correct target field
-- ===========================================================================

-- | A @***@ line sets 'phDescription' to the trimmed value.
prop_descriptionLineSetCorrectly :: ParsedHeader -> Property
prop_descriptionLineSetCorrectly hdr =
  forAll (fmap getSafeString arbitrary) $ \val ->
    case parseHeaderLine hdr ("*** " ++ val) of
      Left _ -> property False
      Right hdr' -> phDescription hdr' === Just val

-- | A @+++@ line sets 'phCategory' to the trimmed value.
prop_categoryLineSetCorrectly :: ParsedHeader -> Property
prop_categoryLineSetCorrectly hdr =
  forAll (fmap getSafeString arbitrary) $ \val ->
    case parseHeaderLine hdr ("+++ " ++ val) of
      Left _ -> property False
      Right hdr' -> phCategory hdr' === Just val

-- | A @---@ tag line appends the value to 'phTags'.
prop_tagLineAppendsCorrectly :: ParsedHeader -> Property
prop_tagLineAppendsCorrectly hdr =
  forAll (fmap getSafeString arbitrary) $ \val ->
    case parseHeaderLine hdr ("--- " ++ val) of
      Left _ -> property False
      Right hdr' -> phTags hdr' === phTags hdr ++ [val]

-- | A @>>>@ line sets 'phWeight' to the parsed integer.
prop_weightLineSetCorrectly :: ParsedHeader -> Property
prop_weightLineSetCorrectly hdr =
  forAll (choose (0, 100 :: Int)) $ \w ->
    case parseHeaderLine hdr (">>> " ++ show w) of
      Left _ -> property False
      Right hdr' -> phWeight hdr' === Just w

-- | A @!I!@ line appends the value to 'phInterpreterCodes'.
prop_interpCodeLineSetCorrectly :: ParsedHeader -> Property
prop_interpCodeLineSetCorrectly hdr =
  forAll (choose (0, 10 :: Int)) $ \c ->
    case parseHeaderLine hdr ("!I! " ++ show c) of
      Left _ -> property False
      Right hdr' -> phInterpreterCodes hdr' === phInterpreterCodes hdr ++ [c]

-- ===========================================================================
-- parseHeaderLine — other fields unchanged
-- ===========================================================================

-- | A @***@ line does not affect any field other than 'phDescription'.
prop_descriptionLinePreservesOthers :: ParsedHeader -> Property
prop_descriptionLinePreservesOthers hdr =
  forAll (fmap getSafeString arbitrary) $ \val ->
    case parseHeaderLine hdr ("*** " ++ val) of
      Left _ -> property False
      Right hdr' ->
        phCategory hdr' === phCategory hdr
          .&&. phTags hdr' === phTags hdr
          .&&. phWeight hdr' === phWeight hdr
          .&&. phParserCodes hdr' === phParserCodes hdr
          .&&. phInterpreterCodes hdr' === phInterpreterCodes hdr

-- | A @+++@ line does not affect any field other than 'phCategory'.
prop_categoryLinePreservesOthers :: ParsedHeader -> Property
prop_categoryLinePreservesOthers hdr =
  forAll (fmap getSafeString arbitrary) $ \val ->
    case parseHeaderLine hdr ("+++ " ++ val) of
      Left _ -> property False
      Right hdr' ->
        phDescription hdr' === phDescription hdr
          .&&. phTags hdr' === phTags hdr
          .&&. phWeight hdr' === phWeight hdr
          .&&. phParserCodes hdr' === phParserCodes hdr
          .&&. phInterpreterCodes hdr' === phInterpreterCodes hdr

-- | A @---@ tag line does not affect any field other than 'phTags'.
prop_tagLinePreservesOtherFields :: ParsedHeader -> Property
prop_tagLinePreservesOtherFields hdr =
  forAll (fmap getSafeString arbitrary) $ \tag ->
    case parseHeaderLine hdr ("--- " ++ tag) of
      Left _ -> property False
      Right hdr' ->
        phDescription hdr' === phDescription hdr
          .&&. phCategory hdr' === phCategory hdr
          .&&. phWeight hdr' === phWeight hdr
          .&&. phParserCodes hdr' === phParserCodes hdr
          .&&. phInterpreterCodes hdr' === phInterpreterCodes hdr

-- | A @!C!@ exit-code line does not affect any field other than 'phParserCodes'.
prop_parserCodeLinePreservesOtherFields :: ParsedHeader -> Property
prop_parserCodeLinePreservesOtherFields hdr =
  case parseHeaderLine hdr "!C! 0" of
    Left _ -> property False
    Right hdr' ->
      phDescription hdr' === phDescription hdr
        .&&. phCategory hdr' === phCategory hdr
        .&&. phTags hdr' === phTags hdr
        .&&. phWeight hdr' === phWeight hdr
        .&&. phInterpreterCodes hdr' === phInterpreterCodes hdr

-- | A @!I!@ exit-code line does not affect any field other than 'phInterpreterCodes'.
prop_interpCodeLinePreservesOthers :: ParsedHeader -> Property
prop_interpCodeLinePreservesOthers hdr =
  forAll (choose (0, 10 :: Int)) $ \c ->
    case parseHeaderLine hdr ("!I! " ++ show c) of
      Left _ -> property False
      Right hdr' ->
        phDescription hdr' === phDescription hdr
          .&&. phCategory hdr' === phCategory hdr
          .&&. phTags hdr' === phTags hdr
          .&&. phWeight hdr' === phWeight hdr
          .&&. phParserCodes hdr' === phParserCodes hdr

-- | A line with an unrecognised prefix leaves the header completely unchanged.
prop_unknownLineIgnored :: ParsedHeader -> Property
prop_unknownLineIgnored hdr =
  forAll unknownLines $ \line ->
    parseHeaderLine hdr line === Right hdr
  where
    -- Lines that don't start with any of the known prefixes
    unknownLines =
      elements
        [ "some random text",
          "# a comment",
          "   ",
          "//comment",
          "abc 123"
        ]

-- ===========================================================================
-- parseHeaderLine — error cases
-- ===========================================================================

-- | A @>>>@ line with a non-integer value yields 'Left'.
prop_invalidWeightIsError :: ParsedHeader -> Bool
prop_invalidWeightIsError hdr =
  case parseHeaderLine hdr ">>> notanumber" of
    Left _ -> True
    Right _ -> False

-- | A @!C!@ line with a non-integer value yields 'Left'.
prop_invalidParserCodeIsError :: ParsedHeader -> Bool
prop_invalidParserCodeIsError hdr =
  case parseHeaderLine hdr "!C! abc" of
    Left _ -> True
    Right _ -> False

-- | A @!I!@ line with a non-integer value yields 'Left'.
prop_invalidInterpCodeIsError :: ParsedHeader -> Bool
prop_invalidInterpCodeIsError hdr =
  case parseHeaderLine hdr "!I! xyz" of
    Left _ -> True
    Right _ -> False

-- ===========================================================================
-- parseHeader
-- ===========================================================================

-- | Multiple @---@ lines accumulate tags in order.
prop_parseHeaderAccumulatesTags :: Property
prop_parseHeaderAccumulatesTags =
  forAll (listOf (fmap getSafeString arbitrary)) $ \tagVals ->
    let lines_ = map ("--- " ++) tagVals
     in case parseHeader lines_ of
          Left _ -> property False
          Right hdr -> phTags hdr === tagVals

-- | An error in any line causes 'parseHeader' to return 'Left', regardless
-- of what follows.
prop_parseHeaderStopsOnFirstError :: Bool
prop_parseHeaderStopsOnFirstError =
  -- The bad weight line causes an error; the +++ line that follows is ignored
  case parseHeader [">>> 1", ">>> bad", "+++ cat"] of
    Left _ -> True
    Right _ -> False

-- | Parsing a set of non-repeatable header lines gives the same result
-- regardless of their order (modulo tag order, which is positional).
-- We use only single-value non-tag lines here.
prop_parseHeaderOrderIndependent :: Property
prop_parseHeaderOrderIndependent =
  forAll (shuffle nonTagLines) $ \shuffled ->
    let ref = parseHeader nonTagLines
        shuffled' = parseHeader shuffled
     in case (ref, shuffled') of
          (Right h1, Right h2) ->
            phCategory h1 === phCategory h2
              .&&. phWeight h1 === phWeight h2
              .&&. phParserCodes h1 === phParserCodes h2
          _ -> property False
  where
    nonTagLines = ["+++ mycat", ">>> 3", "!C! 0"]

-- ===========================================================================
-- determineTestType
-- ===========================================================================

-- | A header with only @!C!@ codes and no @!I!@ codes yields 'ParseOnly'.
prop_parseOnlyFromCCodesOnly :: [Int] -> Property
prop_parseOnlyFromCCodesOnly cs =
  not (null cs) ==>
    let hdr = emptyHeader {phParserCodes = cs}
     in determineTestType hdr === Right ParseOnly

-- | A header with only @!I!@ codes and no @!C!@ codes yields 'ExecuteOnly'.
prop_executeOnlyFromICodesOnly :: [Int] -> Property
prop_executeOnlyFromICodesOnly is =
  not (null is) ==>
    let hdr = emptyHeader {phInterpreterCodes = is}
     in determineTestType hdr === Right ExecuteOnly

-- | A header with @!I!@ codes and explicit @!C! 0@ yields 'Combined'.
prop_combinedFromICodesAndC0 :: [Int] -> Property
prop_combinedFromICodesAndC0 is =
  not (null is) ==>
    let hdr = emptyHeader {phInterpreterCodes = is, phParserCodes = [0]}
     in determineTestType hdr === Right Combined

-- | No codes at all yields 'Left'.
prop_noCodes_givesError :: Property
prop_noCodes_givesError =
  let hdr = emptyHeader
   in property $ case determineTestType hdr of
        Left _ -> True
        Right _ -> False

-- | A header with @!I!@ codes and a non-zero @!C!@ code yields 'Left'.
prop_nonZeroCCodesWithICodesIsError :: [Int] -> Property
prop_nonZeroCCodesWithICodesIsError is =
  not (null is) ==>
    forAll (choose (1, 10 :: Int)) $ \c ->
      let hdr = emptyHeader {phInterpreterCodes = is, phParserCodes = [c]}
       in case determineTestType hdr of
            Left _ -> property True
            Right _ -> property False

-- | A header with @!I!@ codes and @[0, 1]@ @!C!@ codes yields 'Left'
-- (only exactly @[0]@ is permitted for Combined).
prop_mixedCCodesWithICodesIsError :: [Int] -> Property
prop_mixedCCodesWithICodesIsError is =
  not (null is) ==>
    let hdr = emptyHeader {phInterpreterCodes = is, phParserCodes = [0, 1]}
     in case determineTestType hdr of
          Left _ -> property True
          Right _ -> property False

-- ===========================================================================
-- parseTestFile
-- ===========================================================================

-- | A valid file content round-trips through 'parseTestFile'.
-- The parsed 'TestCaseDefinition' has the correct test type.
prop_parseTestFileRoundtrip :: Property
prop_parseTestFileRoundtrip =
  forAll (elements [ParseOnly, ExecuteOnly, Combined]) $ \testType ->
    forAll (genValidTestFileContent testType) $ \(content, tcf) ->
      case parseTestFile tcf content of
        Left err -> counterexample ("Parse failed: " ++ show err) False
        Right def -> tcdTestType def === testType

-- | The parsed 'tcdName' always matches 'tcfName'.
prop_parseTestFilePreservesName :: Property
prop_parseTestFilePreservesName =
  forAll (elements [ParseOnly, ExecuteOnly, Combined]) $ \testType ->
    forAll (genValidTestFileContent testType) $ \(content, tcf) ->
      case parseTestFile tcf content of
        Left _ -> property True -- other failures are tested elsewhere
        Right def -> tcdName def === tcfName tcf

-- | A file without a @+++@ category line yields 'Left'.
prop_missingCategoryIsError :: Property
prop_missingCategoryIsError =
  -- A ParseOnly file with weight and !C! but no +++
  let content = ">>> 1\n!C! 0\n\nsome source code\n"
      tcf = TestCaseFile "test" "test.test" Nothing Nothing
   in case parseTestFile tcf content of
        Left _ -> property True
        Right _ -> property False

-- | A file without a @>>>@ weight line yields 'Left'.
prop_missingWeightIsError :: Property
prop_missingWeightIsError =
  let content = "+++ mycat\n!C! 0\n\nsome source code\n"
      tcf = TestCaseFile "test" "test.test" Nothing Nothing
   in case parseTestFile tcf content of
        Left _ -> property True
        Right _ -> property False

-- | For 'ParseOnly' tests, 'tcdExpectedParserExitCodes' is 'Just' and
-- 'tcdExpectedInterpreterExitCodes' is 'Nothing'.
prop_parseOnlyExitCodesCorrect :: Property
prop_parseOnlyExitCodesCorrect =
  forAll (genValidTestFileContent ParseOnly) $ \(content, tcf) ->
    case parseTestFile tcf content of
      Left _ -> property True
      Right def ->
        property (isJust (tcdExpectedParserExitCodes def))
          .&&. property (isNothing (tcdExpectedInterpreterExitCodes def))

-- | For 'ExecuteOnly' tests, 'tcdExpectedInterpreterExitCodes' is 'Just' and
-- 'tcdExpectedParserExitCodes' is 'Nothing'.
prop_executeOnlyExitCodesCorrect :: Property
prop_executeOnlyExitCodesCorrect =
  forAll (genValidTestFileContent ExecuteOnly) $ \(content, tcf) ->
    case parseTestFile tcf content of
      Left _ -> property True
      Right def ->
        property (isJust (tcdExpectedInterpreterExitCodes def))
          .&&. property (isNothing (tcdExpectedParserExitCodes def))

-- ===========================================================================
-- Aggregate
-- ===========================================================================

-- | Run all parser properties.
parserProps :: IO ()
parserProps = do
  putStrLn "=== Parser properties ==="
  -- splitHeaderBody
  quickCheck (withMaxSuccess 200 prop_splitPreservesHeader)
  quickCheck (withMaxSuccess 200 prop_noBlankLineEmptyBody)
  quickCheck (withMaxSuccess 200 prop_splitPreservesContent)
  quickCheck (withMaxSuccess 200 prop_splitBodyPreservesContent)
  quickCheck prop_splitEmptyInput
  -- parseHeaderLine: correct target field
  quickCheck (withMaxSuccess 200 prop_descriptionLineSetCorrectly)
  quickCheck (withMaxSuccess 200 prop_categoryLineSetCorrectly)
  quickCheck (withMaxSuccess 200 prop_tagLineAppendsCorrectly)
  quickCheck (withMaxSuccess 200 prop_weightLineSetCorrectly)
  quickCheck (withMaxSuccess 200 prop_interpCodeLineSetCorrectly)
  -- parseHeaderLine: other fields unchanged
  quickCheck (withMaxSuccess 200 prop_descriptionLinePreservesOthers)
  quickCheck (withMaxSuccess 200 prop_categoryLinePreservesOthers)
  quickCheck (withMaxSuccess 200 prop_tagLinePreservesOtherFields)
  quickCheck (withMaxSuccess 200 prop_parserCodeLinePreservesOtherFields)
  quickCheck (withMaxSuccess 200 prop_interpCodeLinePreservesOthers)
  quickCheck (withMaxSuccess 200 prop_unknownLineIgnored)
  -- parseHeaderLine: error cases
  quickCheck (withMaxSuccess 200 prop_invalidWeightIsError)
  quickCheck (withMaxSuccess 200 prop_invalidParserCodeIsError)
  quickCheck (withMaxSuccess 200 prop_invalidInterpCodeIsError)
  -- parseHeader
  quickCheck (withMaxSuccess 200 prop_parseHeaderAccumulatesTags)
  quickCheck prop_parseHeaderStopsOnFirstError
  quickCheck (withMaxSuccess 200 prop_parseHeaderOrderIndependent)
  -- determineTestType
  quickCheck (withMaxSuccess 200 prop_parseOnlyFromCCodesOnly)
  quickCheck (withMaxSuccess 200 prop_executeOnlyFromICodesOnly)
  quickCheck (withMaxSuccess 200 prop_combinedFromICodesAndC0)
  quickCheck prop_noCodes_givesError
  quickCheck (withMaxSuccess 200 prop_nonZeroCCodesWithICodesIsError)
  quickCheck (withMaxSuccess 200 prop_mixedCCodesWithICodesIsError)
  -- parseTestFile
  quickCheck (withMaxSuccess 200 prop_parseTestFileRoundtrip)
  quickCheck (withMaxSuccess 200 prop_parseTestFilePreservesName)
  quickCheck prop_missingCategoryIsError
  quickCheck prop_missingWeightIsError
  quickCheck (withMaxSuccess 200 prop_parseOnlyExitCodesCorrect)
  quickCheck (withMaxSuccess 200 prop_executeOnlyExitCodesCorrect)
