Tasty Travis: Fancy Travis CI output for tasty tests
====================================================
[![BSD3](https://img.shields.io/badge/License-BSD-blue.svg)](https://en.wikipedia.org/wiki/BSD_License)
[![Hackage](https://img.shields.io/hackage/v/tasty-travis.svg)](https://hackage.haskell.org/package/tasty-travis)
[![Build Status](https://travis-ci.org/merijn/tasty-travis.svg)](https://travis-ci.org/merijn/tasty-travis)

**Tasty Travis** provides fancy
[Tasty](https://hackage.haskell.org/package/tasty) test output on
[Travis CI](https://travis-ci.org/).

It allows you get coloured test output, folding and collapsing groups of tests,
and hiding the output of successful tests.

Example
-------

Here's what an example `test.hs` might look:

```haskell
import Test.Tasty
import Test.Tasty.Travis (travisTestReporter, defaultConfig, listingTests)
import Test.Tasty.HUnit

import Data.List
import Data.Ord

main = defaultMainWithIngredients ingredients tests
  where
    ingredients = [ listingTests, travisTestReporter defaultConfig ]

tests :: TestTree
tests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]
```
