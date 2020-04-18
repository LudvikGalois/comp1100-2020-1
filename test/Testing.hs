module Testing where

import System.Exit

-- | A 'Test' is a 'String' label identifying the thing being tested,
-- and the result of the test after running it. We give labels to
-- tests so we have some idea what they mean.
data Test = Test String TestResult

-- | A test either passes, or fails with an error message.
data TestResult = OK | Fail String

-- | Test that two things are equal. The first argument is the
-- computed result, the second argument is the value it should be
-- equal to.
assertEqual :: (Eq a, Show a) => a -> a -> TestResult
assertEqual actual expected
  | actual == expected = OK
  | otherwise = Fail (show actual ++ " is not equal to\n" ++ show expected)

-- | Test that two things are different. The first argument is the
-- computed result, the second argument is the value it should be
-- different from.
assertNotEqual :: (Eq a, Show a) => a -> a -> TestResult
assertNotEqual actual expected
  | actual /= expected = OK
  | otherwise = Fail (show actual ++ " is equal to\n" ++ show expected)

-- | Test that two 'Double's are basically equal. The first argument
-- is the computed result, the second argument is the value it should be
-- close to.
assertApproxEqual :: Double -> Double -> TestResult
assertApproxEqual actual expected
  | abs (actual - expected) < 0.0001 = OK
  | otherwise =
    Fail (show actual ++ " is not approx. equal to\n" ++ show expected)

-- | Run a list of tests. You are not expected to understand how this
-- works.
runTests :: [Test] -> IO ()
runTests [] = exitSuccess
runTests ((Test msg OK):ts) = do
  putStrLn ("PASS: " ++ msg)
  runTests ts
runTests ((Test msg (Fail failMsg)):_) = do
  putStrLn ("FAIL: " ++ msg)
  putStrLn failMsg
  exitFailure
