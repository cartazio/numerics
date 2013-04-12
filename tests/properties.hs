

module Main where



import Data.Monoid (mempty)
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
--import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
--import Test.HUnit

import Data.List

import Data.Numerics.Simple.Bits

main = defaultMain tests

--mainWithOpts = do
--  -- Test options can also be specified in the code. The TestOptions
--  -- type is an instance of the Monoid type class, so the easiest way
--  -- to get an empty set of options is with `mempty`.
--  let empty_test_opts = mempty :: TestOptions

--  -- We update the empty TestOptions with our desired values.
--  let my_test_opts = empty_test_opts {
--    topt_maximum_generated_tests = Just 500
--  }

--  -- Now we create an empty RunnerOptions in the same way, and add
--  -- our TestOptions to it.
--  let empty_runner_opts = mempty :: RunnerOptions
--  let my_runner_opts = empty_runner_opts {
--    ropt_test_options = Just my_test_opts
--  }

--  defaultMainWithOpts tests my_runner_opts

--tests = [
--        testGroup "Sorting Group 1" [
--                testProperty "sort1" prop_sort1,
--                testProperty "sort2" prop_sort2,
--                testProperty "sort3" prop_sort3
--            ],
--        testGroup "Sorting Group 2" [
--                testGroup "Nested Group 1" [
--                       testProperty "sort4" prop_sort4,
--                       testProperty "sort5" prop_sort5,
--                       testProperty "sort6" prop_sort6
--                     ],
--                testProperty "sort7" prop_sort7,
--                testCase "sort8" test_sort8,
--                testCase "sort9" test_sort9
--            ]
--    ]


tests = [ 
    testGroup "MortonZ Shuffles" [
        testProperty "shuffle unshuffle is identity" prop_mortonOuterId,
        testProperty "both shuffles agree" prop_mortonOuterShufflesAgree,
        testProperty "both unshuffles agree " prop_mortonOuterUnshufflesAgree
        ]

    ] --- need to add some tests! 


composeIsId x f finv = x == (finv $! f x )

prop_mortonOuterId x =  composeIsId x outerShuffle64A outerUnShuffle64A  .&&. 
            composeIsId x outerShuffle64B outerUnShuffle64B

prop_mortonOuterUnshufflesAgree x =   outerUnShuffle64B x == outerUnShuffle64A x

prop_mortonOuterShufflesAgree x = outerShuffle64B x == outerShuffle64A x 