sortIntegers :: (Int, Int, Int) -> (Int, Int, Int)
sortIntegers (a, b, c) = (min a (min b c), middle, max a (max b c))
  where
    middle = if a >= min b c && a <= max b c then a
             else if b >= min a c && b <= max a c then b
             else c
check :: ((Int, Int, Int) -> (Int, Int, Int)) -> IO ()
check sortIntegersFunc = do
  let testCases =
        [ ((1, 14, 5), (1, 5, 14)),
          ((2, 2, 2), (2, 2, 2)),
          ((9, 4, 7), (4, 7, 9)),
          ((15, 11, 12), (11, 12, 15)),
          ((5, 3, 8), (3, 5, 8))
        ]

  mapM_ (\(inputs, expected) -> testCase inputs expected) testCases
  where
    testCase inputs expected = do
      let result = sortIntegersFunc inputs
      putStrLn $
        "Input: " ++ show inputs ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check sortIntegers