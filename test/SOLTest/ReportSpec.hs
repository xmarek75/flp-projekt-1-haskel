-- | QuickCheck property tests for 'SOLTest.Report'.
module SOLTest.ReportSpec
  ( reportProps,
  )
where

import Data.Map.Strict qualified as Map
import SOLTest.Generators (arbitraryCategoryReport)
import SOLTest.Report
import SOLTest.Types
import Test.QuickCheck

-- ---------------------------------------------------------------------------
-- rateToBin properties
-- ---------------------------------------------------------------------------

-- | 'rateToBin' always returns one of the ten valid bin keys.
prop_rateToBinValidKey :: Property
prop_rateToBinValidKey =
  forAll (choose (0.0, 1.0)) $ \r ->
    rateToBin r `elem` validBins

-- | A rate of exactly @1.0@ maps to the @\"0.9\"@ bin (the last bin).
prop_perfectRateBin :: Bool
prop_perfectRateBin = rateToBin 1.0 == "0.9"

-- | A rate of @0.0@ maps to the @\"0.0\"@ bin.
prop_zeroBin :: Bool
prop_zeroBin = rateToBin 0.0 == "0.0"

-- | For any rate r in [0.0, 0.1), the bin is @\"0.0\"@.
prop_firstBinRange :: Property
prop_firstBinRange =
  forAll (choose (0.0, 0.099)) $ \r ->
    rateToBin r == "0.0"

-- | For any rate r in [0.9, 1.0], the bin is @\"0.9\"@.
prop_lastBinRange :: Property
prop_lastBinRange =
  forAll (choose (0.9, 1.0)) $ \r ->
    rateToBin r == "0.9"

-- ---------------------------------------------------------------------------
-- computeHistogram properties
-- ---------------------------------------------------------------------------

-- | The sum of all histogram bin counts equals the number of categories.
prop_histogramSumEqualsCategories :: Property
prop_histogramSumEqualsCategories =
  forAll (listOf arbitraryCategoryReport) $ \pairs ->
    let cats = Map.fromList pairs
        hist = computeHistogram cats
     in sum (Map.elems hist) === Map.size cats

-- | 'computeHistogram' always produces exactly ten bins.
prop_histogramHasTenBins :: Property
prop_histogramHasTenBins =
  forAll (listOf arbitraryCategoryReport) $ \pairs ->
    let cats = Map.fromList pairs
        hist = computeHistogram cats
     in Map.keys hist === validBins

-- | An empty category map produces a histogram with all zeros.
prop_emptyHistogramAllZero :: Bool
prop_emptyHistogramAllZero =
  all (== 0) (Map.elems (computeHistogram Map.empty))

-- ---------------------------------------------------------------------------
-- computeStats properties
-- ---------------------------------------------------------------------------

-- | The 'tsFoundTestFiles' field always reflects the @foundCount@ argument.
prop_statsPreservesFoundCount :: Int -> Property
prop_statsPreservesFoundCount n =
  forAll (listOf arbitraryCategoryReport) $ \pairs ->
    let stats = computeStats n 0 0 (Just (Map.fromList pairs))
     in tsFoundTestFiles stats === n

-- | The 'tsLoadedTests' field always reflects the @loadedCount@ argument.
prop_statsPreservesLoadedCount :: Int -> Property
prop_statsPreservesLoadedCount n =
  forAll (listOf arbitraryCategoryReport) $ \pairs ->
    let stats = computeStats 0 n 0 (Just (Map.fromList pairs))
     in tsLoadedTests stats === n

-- | The 'tsSelectedTests' field always reflects the @selectedCount@ argument.
prop_statsPreservesSelectedCount :: Int -> Property
prop_statsPreservesSelectedCount n =
  forAll (listOf arbitraryCategoryReport) $ \pairs ->
    let stats = computeStats 0 0 n (Just (Map.fromList pairs))
     in tsSelectedTests stats === n

-- | In dry-run mode (@Nothing@ results), 'tsPassedTests' is always 0.
prop_statsNothingZeroPassed :: Int -> Int -> Int -> Bool
prop_statsNothingZeroPassed f l s =
  tsPassedTests (computeStats f l s Nothing) == 0

-- | In dry-run mode, the histogram has all bins set to zero.
prop_statsNothingEmptyHistogram :: Int -> Int -> Int -> Bool
prop_statsNothingEmptyHistogram f l s =
  all (== 0) (Map.elems (tsHistogram (computeStats f l s Nothing)))

-- | When results are provided, 'tsPassedTests' equals the total count of
-- 'Passed' results across all categories.
prop_statsPassedCountCorrect :: Property
prop_statsPassedCountCorrect =
  forAll (listOf arbitraryCategoryReport) $ \pairs ->
    let cats = Map.fromList pairs
        stats = computeStats 0 0 0 (Just cats)
        -- Count Passed results directly from the category map
        expected =
          sum
            [ Map.size (Map.filter (\r -> tcrResult r == Passed) (crTestResults cr))
              | cr <- Map.elems cats
            ]
     in tsPassedTests stats === expected

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | The ten valid bin keys in order.
validBins :: [String]
validBins = ["0.0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9"]

-- ---------------------------------------------------------------------------
-- Aggregate
-- ---------------------------------------------------------------------------

-- | Run all report properties.
reportProps :: IO ()
reportProps = do
  putStrLn "=== Report properties ==="
  quickCheck prop_rateToBinValidKey
  quickCheck prop_perfectRateBin
  quickCheck prop_zeroBin
  quickCheck prop_firstBinRange
  quickCheck prop_lastBinRange
  quickCheck (withMaxSuccess 200 prop_histogramSumEqualsCategories)
  quickCheck (withMaxSuccess 200 prop_histogramHasTenBins)
  quickCheck prop_emptyHistogramAllZero
  quickCheck (withMaxSuccess 200 prop_statsPreservesFoundCount)
  quickCheck (withMaxSuccess 200 prop_statsPreservesLoadedCount)
  quickCheck (withMaxSuccess 200 prop_statsPreservesSelectedCount)
  quickCheck prop_statsNothingZeroPassed
  quickCheck prop_statsNothingEmptyHistogram
  quickCheck (withMaxSuccess 200 prop_statsPassedCountCorrect)
