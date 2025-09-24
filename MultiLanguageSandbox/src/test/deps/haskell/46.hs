reverseThreeDigit :: Int -> Int
reverseThreeDigit n = (n `mod` 10) * 100 + ((n `div` 10) `mod` 10) * 10 + (n `div` 100)
check :: (Int -> Int) -> IO ()
check reverseThreeDigitFunc = do
  let testCases = 
        [ (358, 853),
          (100, 1),
          (678, 876),
          (250, 52),
          (999, 999)
        ]

  mapM_ (\(number, expected) -> testCase number expected) testCases
  where
    testCase number expected = do
      let result = reverseThreeDigitFunc number
      putStrLn $
        "Input: " ++ show number ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check reverseThreeDigit