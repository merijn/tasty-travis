import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Travis

main :: IO ()
main = travisTestReporter travisConfig [] . testGroup "tasty-travis" $
    [ testGroup "Group 1"
        [ testCase "Test 1" $ return ()
        , testCase "Test 2" $ return ()
        , testCase "Test 3" $ return ()
        , testCase "Test 4" $ return ()
        ]
    , testGroup "Group 2" [ testCase "Test 1" $ return () ]
    , testGroup "Group 3"
        [ testCase "Test 0" $ return ()
        , testGroup "Group 4" $
            [ testCase "Test 1" $ return ()
            , testCase "Test 2" $ return ()
            , testCase "Test 3" $ return ()
            , testCase "Test 4" $ return ()
            ]
        ]
    , testGroup "Group 4" [ testCase "Test 1" $ return ()]
    ]
  where
    travisConfig = defaultConfig
      { travisFoldGroup = FoldMoreThan 2
      , travisSummaryWhen = SummaryAlways
      }
