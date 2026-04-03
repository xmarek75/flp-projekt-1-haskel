-- | QuickCheck property tests for 'SOLTest.Filter'.
module SOLTest.FilterSpec
  ( filterProps,
  )
where

import Data.List (sort)
import SOLTest.Filter ( filterTests, matchesCriterion )
import SOLTest.Generators ()
import SOLTest.Types
    ( FilterCriterion(ByAny, ByCategory, ByTag),
      FilterSpec(..),
      TestCaseDefinition(tcdName, tcdCategory, tcdTags) )
import Test.QuickCheck

-- ---------------------------------------------------------------------------
-- Basic filter properties
-- ---------------------------------------------------------------------------

-- | When no include and no exclude criteria are given, all tests are selected.
prop_emptyFilterSelectsAll :: [TestCaseDefinition] -> Property
prop_emptyFilterSelectsAll tests =
  let spec = FilterSpec [] [] False
      (selected, excl) = filterTests spec tests
   in property (selected == tests && null excl)

-- | Filtering is idempotent: re-applying the same filter to the already-
-- filtered result produces the same selection.
prop_filterIdempotent :: FilterSpec -> [TestCaseDefinition] -> Property
prop_filterIdempotent spec tests =
  let (selected1, _) = filterTests spec tests
      (selected2, _) = filterTests spec selected1
   in selected1 === selected2

-- | Filtering partitions the input: selected ++ filteredOut (by name) covers
-- all input tests.
prop_filterPartitions :: FilterSpec -> [TestCaseDefinition] -> Property
prop_filterPartitions spec tests =
  let (selected, filteredOut) = filterTests spec tests
      allNames = sort (map tcdName tests)
      resultNames = sort (map tcdName selected ++ map tcdName filteredOut)
   in allNames === resultNames

-- | A test excluded by category is never in the selected set, even if it
-- matches an include criterion for the same category.
prop_excludeOverridesInclude :: TestCaseDefinition -> Property
prop_excludeOverridesInclude test =
  let cat = tcdCategory test
      spec =
        FilterSpec
          { fsIncludes = [ByCategory cat],
            fsExcludes = [ByCategory cat],
            fsUseRegex = False
          }
      (selected, _) = filterTests spec [test]
   in property (test `notElem` selected)

-- | A test that matches an include-by-tag criterion is included when there
-- are no excludes.
prop_includeByTagWorks :: TestCaseDefinition -> Property
prop_includeByTagWorks test =
  not (null (tcdTags test)) ==>
    let tag = head (tcdTags test)
        spec = FilterSpec {fsIncludes = [ByTag tag], fsExcludes = [], fsUseRegex = False}
        (selected, _) = filterTests spec [test]
     in property (test `elem` selected)

-- | A test that matches an exclude-by-name criterion is never selected.
prop_excludeByNameWorks :: TestCaseDefinition -> Property
prop_excludeByNameWorks test =
  let name = tcdName test
      spec = FilterSpec {fsIncludes = [], fsExcludes = [ByAny name], fsUseRegex = False}
      (selected, _) = filterTests spec [test]
   in property (test `notElem` selected)

-- ---------------------------------------------------------------------------
-- matchesCriterion properties
-- ---------------------------------------------------------------------------

-- | A 'ByCategory' criterion matches a test whose category equals the value.
prop_byCategoryMatchesCategory :: TestCaseDefinition -> Property
prop_byCategoryMatchesCategory test =
  property $
    matchesCriterion False test (ByCategory (tcdCategory test))

-- | A 'ByTag' criterion matches a test that has that tag.
prop_byTagMatchesTag :: TestCaseDefinition -> Property
prop_byTagMatchesTag test =
  not (null (tcdTags test)) ==>
    matchesCriterion False test (ByTag (head (tcdTags test)))

-- | A 'ByAny' criterion matches a test whose name equals the value.
prop_byAnyMatchesName :: TestCaseDefinition -> Property
prop_byAnyMatchesName test =
  property $
    matchesCriterion False test (ByAny (tcdName test))

-- ---------------------------------------------------------------------------
-- Aggregate
-- ---------------------------------------------------------------------------

-- | Run all filter properties.
filterProps :: IO ()
filterProps = do
  putStrLn "=== Filter properties ==="
  quickCheck (withMaxSuccess 200 prop_emptyFilterSelectsAll)
  quickCheck (withMaxSuccess 200 prop_filterIdempotent)
  quickCheck (withMaxSuccess 200 prop_filterPartitions)
  quickCheck (withMaxSuccess 200 prop_excludeOverridesInclude)
  quickCheck (withMaxSuccess 200 prop_includeByTagWorks)
  quickCheck (withMaxSuccess 200 prop_excludeByNameWorks)
  quickCheck (withMaxSuccess 200 prop_byCategoryMatchesCategory)
  quickCheck (withMaxSuccess 200 prop_byTagMatchesTag)
  quickCheck (withMaxSuccess 200 prop_byAnyMatchesName)
