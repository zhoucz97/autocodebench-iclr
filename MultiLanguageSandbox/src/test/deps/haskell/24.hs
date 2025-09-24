import Data.Char (digitToInt)

countDigitInFactorial :: Int -> Int -> Int
countDigitInFactorial n a = countDigit (factorial n) a
  where
    factorial :: Int -> Integer
    factorial 0 = 1
    factorial k = fromIntegral k * factorial (k - 1)
    
    countDigit :: Integer -> Int -> Int
    countDigit num digit = length $ filter (== digit) (map digitToInt (show num))
check :: (Int -> Int -> Int) -> IO ()
check countDigitInFactorialFunc = do
  let testCases =
        [ ((4, 2), 1),
          ((5, 1), 1),
          ((10, 0), 2),
          ((6, 3), 0),
          ((9, 5), 0)
        ]

  mapM_ (\((n, a), expected) -> testCase n a expected) testCases
  where
    testCase n a expected = do
      let result = countDigitInFactorialFunc n a
      putStrLn $
        "Input: n = " ++ show n ++ ", a = " ++ show a ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check countDigitInFactorial