intBoolIntConversion :: Int -> Int
intBoolIntConversion x = if x /= 0 then 1 else 0
check :: (Int -> Int) -> IO ()
check intBoolIntConversionFunc = do
  let testCases = 
        [ (3, 1),
          (0, 0),
          (-5, 1),
          (100, 1),
          (-100, 1)
        ]

  mapM_ (\(number, expected) -> testCase number expected) testCases
  where
    testCase number expected = do
      let result = intBoolIntConversionFunc number
      putStrLn $
        "Input: " ++ show number ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check intBoolIntConversion