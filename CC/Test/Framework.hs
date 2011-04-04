module CC.Test.Framework (
    module Test.Framework,
    module Test.Framework.TH,
    module Test.Framework.Providers.HUnit,
    module Test.Framework.Providers.QuickCheck2,
    module Test.HUnit,
    module Test.QuickCheck,
    testSame,testSames,testSames',testNoneSome,testAllNone
  ) where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 -- requires QuickCheck-2.1.1.1
import Test.HUnit hiding (Test,Testable)
import Test.QuickCheck

import Data.Set (Set,empty,fromList)

-- There are two implementations of this function.  The first condenses all
-- cases into a single test, which makes for nicer test output.  The second
-- runs each case separately, which is useful for debugging a failed test
testSame :: (Eq a, Show a) => String -> [a] -> [a] -> [Test]
testSame n as es = terse -- verbose
  where terse   = [testCase n $ as @?= take (length as) es]
        verbose = zipWith testCase [n++" "++show i | i <- [0..]]
                                   (zipWith (@?=) as es)

testSames :: (Eq b, Show b) => String -> (a -> b) -> [String] -> [[a]] -> [[b]] -> [Test]
testSames n f ns ass bss = concat $ zipWith3 entry ns ass bss
  where entry m as bs = testSame (n ++ " " ++ m) (map f as) bs

testSames' :: (Eq b, Show b) => String -> (a -> b) -> [String] -> [[a]] -> [b] -> [Test]
testSames' n f ns ass bs = testSames n f ns ass
                           [replicate (length as) b | (as,b) <- zip ass bs]

testNoneSome :: (Ord b, Show b) => String -> (a -> Set b) -> [a] -> [a] -> [[b]] -> [Test]
testNoneSome n f nones somes rs = testSames n f ["none","some"] [nones,somes]
                                  [repeat empty, map fromList rs]

testAllNone :: String -> (a -> Bool) -> [a] -> [a] -> [Test]
testAllNone n f alls nones = testSames' n f ["yes","no "] [alls,nones] [True,False]
