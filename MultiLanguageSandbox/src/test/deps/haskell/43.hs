powerOfTwo :: Int -> Int
powerOfTwo n = 2 ^ n
check :: (Int -> Int) -> IO ()
check powerOfTwoFunc = do
  let testCases = 
        [ (3, 8),
          (5, 32),
          (0, 1),
          (10, 1024),
          (30, 1073741824)
        ]

  mapM_ (\(number, expected) -> testCase number expected) testCases
  where
    testCase number expected = do
      let result = powerOfTwoFunc number
      putStrLn $
        "Input: " ++ show number ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check powerOfTwo