collatzSequence :: Int -> [Int]
collatzSequence n = reverse $ takeWhile (/= 1) (iterate next n) ++ [1]
  where
    next x
      | odd x     = 3 * x + 1
      | otherwise = x `div` 2
check :: (Int -> [Int]) -> IO ()
check collatzSequenceFunc = do
  let testCases =
        [ (6, [1, 2, 4, 8,16, 5, 10, 3, 6]),
          (15, [1,2,4,8,16,5,10,20,40,80,160,53,106,35,70,23,46,15]),
          (1, [1])
        ]

  mapM_ (\(input, expected) -> testCase input expected) testCases
  where
    testCase input expected = do
      let result = collatzSequenceFunc input
      putStrLn $
        "Input: " ++ show input ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check collatzSequence