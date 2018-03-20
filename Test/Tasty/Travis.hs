{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Trustworthy #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Test.Tasty.Travis
-- Copyright   :  (C) 2017 Merijn Verstraaten
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Merijn Verstraaten <merijn@inconsistent.nl>
-- Stability   :  experimental
-- Portability :  haha
--
-- <https://travis-ci.org/ Travis CI> support for
-- <https://hackage.haskell.org/package/tasty tasty>.
--
-- This module provides a tasty test reporter which functions as a drop-in
-- replacement for 'defaultMainWithIngredients' from tasty. It detects whether
-- the \"TRAVIS\" environment variable is set to \"true\". If so, it produces
-- output as configured. If not, it falls back to the 'consoleTestReporter'.
-------------------------------------------------------------------------------
module Test.Tasty.Travis
    ( travisTestReporter
    , TravisConfig(..)
    , defaultConfig
    , FoldGroup(..)
    , FoldWhen(..)
    , SummaryWhen(..)
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative(..), (<$>), (<$), (<*>))
import Data.Monoid (Monoid(..))
#endif
import Control.Monad (when)
import Data.Char (isSpace)
import Data.Monoid (Sum(..))
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup(..))
#endif
import System.Environment (lookupEnv)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)

import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.Options (IsOption(..), OptionSet, setOption)
import Test.Tasty.Runners

newtype WrapIO a = WrapIO { unwrapIO :: IO a }
    deriving (Applicative, Functor, Monad)

#if MIN_VERSION_base(4,9,0)
instance Semigroup a => Semigroup (WrapIO a) where
    x <> y = (<>) <$> x <*> y
#endif

instance Monoid a => Monoid (WrapIO a) where
    mempty = WrapIO $ return mempty
    mappend x y = mappend <$> x <*> y

-- | Configuration for the output generated on Travis CI.
data TravisConfig
    = TravisConfig
    { travisQuiet :: Bool
    -- ^ Do not report individual tests, only overall pass/fail Statistics.
    , travisHideSuccesses :: Bool
    -- ^ If 'True', only report failures.
    , travisUseColour :: Bool
    -- ^ If 'True', generate coloured output
    , travisFoldGroup :: FoldGroup
    -- ^ How to group folds.
    , travisFoldWhen :: FoldWhen
    -- ^ When to fold.
    , travisSummaryWhen :: SummaryWhen
    -- ^ When to print a summary for a fold.
    , travisTestOptions :: OptionSet -> OptionSet
    -- ^ Set/unset options when running on Travis. Functions like
    -- 'PlusTestOptions' when running on Travis, does nothing otherwise.
    }

-- | Default Travis configuration. Coloured output. Folds all successes away.
-- Adds summaries for folds with failures (which don't happen in this config).
defaultConfig :: TravisConfig
defaultConfig = TravisConfig
    { travisQuiet = quiet
    , travisHideSuccesses = hide
    , travisUseColour = True
    , travisFoldGroup = FoldAll
    , travisFoldWhen = FoldSuccess
    , travisSummaryWhen = SummaryFailures
    , travisTestOptions = id
    } where
        HideSuccesses hide = defaultValue
        Quiet quiet = defaultValue

-- | Control which parts of the test tree are folded away.
data FoldGroup
    = FoldMoreThan Int
    -- ^ Fold groups with more than N entries (groups or tests).
    | FoldBelow Int
    -- ^ Fold groups more than N levels from the root.
    | FoldTop Int
    -- ^ Fold groups N or less levels from the root.
    | FoldAll
    -- ^ Fold all groups.
    deriving (Eq, Show)

-- | Control when the tests/groups specified by 'FoldGroup' are folded.
data FoldWhen
    = FoldNever -- ^ Never fold output.
    | FoldSuccess -- ^ Fold all groups that have 0 failures.
    | FoldAlways -- ^ Always fold groups.
    deriving (Eq, Show)

-- | Control when a summary is printed before a fold.
data SummaryWhen
    = SummaryNever -- ^ Never print summaries.
    | SummaryFailures -- ^ Print summaries before folds with failures.
    | SummaryAlways -- ^ Always print summaries before folds.
    deriving (Eq, Show)

-- | A Tasty test runner whose output can be controlled with a 'TravisConfig'.
-- Defaults to the regular console reporting when the TRAVIS environment
-- variable is not set to \"true\".
--
-- Usage:
--
-- @'travisTestReporter' yourConfig [] yourTestTree@
travisTestReporter :: TravisConfig -> [Ingredient] -> TestTree -> IO ()
travisTestReporter cfg@TravisConfig{..} ingredients tests = do
    isTravis <- maybe False (=="true") <$> lookupEnv "TRAVIS"
    let finalIngredients
            | isTravis = ingredients ++ [listingTests, travisReporter]
            | otherwise = ingredients ++ [listingTests, consoleTestReporter]

        tree | isTravis = PlusTestOptions travisTestOptions tests
             | otherwise = tests

    defaultMainWithIngredients finalIngredients tree
  where
    TestReporter baseOpts _ = consoleTestReporter

    travisReporter :: Ingredient
    travisReporter = TestReporter baseOpts runTests

    runTests :: OptionSet -> TestTree
             -> Maybe (StatusMap -> IO (Time -> IO Bool))
    runTests opts tree = Just $ \smap ->
        runTravisTestReporter cfg travisOptions tree smap
      where
        travisOptions :: OptionSet
        travisOptions = setOption (Quiet travisQuiet)
                      . setOption (HideSuccesses travisHideSuccesses)
                      . setOption (if travisUseColour then Always else Auto)
                      $ opts

runTravisTestReporter
    :: TravisConfig
    -> OptionSet
    -> TestTree
    -> StatusMap
    -> IO (Time -> IO Bool)
runTravisTestReporter cfg@TravisConfig{..} opts tree smap = do
    let ?colors = travisUseColour
    let testOutput = buildTestOutput opts tree

    hSetBuffering stdout LineBuffering
    (output, stats) <- travisOutput cfg testOutput smap
    when (not travisQuiet) $ unwrapIO $ output "" 0
    return $ \time ->
        (statFailures stats == 0) <$ printStatistics stats time

travisOutput
    :: (?colors :: Bool)
    => TravisConfig
    -> TestOutput
    -> StatusMap
    -> IO (String -> Int -> WrapIO (), Statistics)
travisOutput TravisConfig{..} output smap =
    fmap strip . unwrapIO $ foldTestOutput foldTest foldHeading output smap
  where
    strip (x,y,_) = (x,y)
    foldTest
        :: String
        -> IO ()
        -> IO Result
        -> (Result -> IO ())
        -> WrapIO (String -> Int -> WrapIO (), Statistics, Sum Int)
    foldTest _name printName getResult printResult = WrapIO $ do
        r <- getResult
        return $ case resultOutcome r of
                Success -> (success r, Statistics 1 0, Sum 1)
                Failure{} -> (doPrint r, Statistics 1 1, Sum 1)
      where
        success r | travisHideSuccesses = \_ _ -> return ()
                  | otherwise = doPrint r

        doPrint r _ _ = WrapIO $ printName >> printResult r

    foldHeading
        :: String
        -> IO ()
        -> WrapIO (String -> Int -> WrapIO (), Statistics, Sum Int)
        -> WrapIO (String -> Int -> WrapIO (), Statistics, Sum Int)
    foldHeading name printHeading foldBody = do
        (printBody, stats@Statistics{..}, kids) <- foldBody
        let act label n = WrapIO $ do
                when mustFold $
                    putStrLn $ "travis_fold:start:" ++ foldMarker ++ "\\r"

                if mustSummarise
                   then do
                       putStr $ replicate (2*n) ' ' ++ name ++ ": "
                       printStatisticsNoTime stats
                   else printHeading

                unwrapIO $ printBody (foldMarker ++ ".") (n+1)

                when mustFold $
                    putStrLn $ "travis_fold:end:" ++ foldMarker ++ "\\r"
              where
                replace c | isSpace c = '_'
                          | otherwise = c

                foldMarker = label ++ map replace name
                mustFold = doFold travisFoldWhen stats travisFoldGroup kids n
                mustSummarise = and [ n /= 0, mustFold
                                    , doSummary travisSummaryWhen stats]

        if statTotal == 0 || (statFailures == 0 && travisHideSuccesses)
           then return (\_ _ -> return (), stats, Sum 0)
           else return (act, stats, Sum 1)

doFold :: FoldWhen -> Statistics -> FoldGroup -> Sum Int -> Int -> Bool
doFold FoldNever _ = \_ _ _ -> False
doFold FoldSuccess stats
    | statFailures stats == 0 = doFoldGroup
    | otherwise = \_ _ _ -> False
doFold FoldAlways _ = doFoldGroup

doFoldGroup :: FoldGroup -> Sum Int -> Int -> Bool
doFoldGroup FoldAll _ _ = True
doFoldGroup (FoldBelow n) _ i = i > n
doFoldGroup (FoldTop n) _ i = i <= n
doFoldGroup (FoldMoreThan n) kids _ = getSum kids > n

doSummary :: SummaryWhen -> Statistics -> Bool
doSummary SummaryNever _ = False
doSummary SummaryFailures stats = statFailures stats /= 0
doSummary SummaryAlways _ = True
