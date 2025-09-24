import Data.List (sort)

hasCloseElements :: [Float] -> Float -> Bool
hasCloseElements xs threshold = any (< threshold) (zipWith (-) (tail sorted) sorted)
  where sorted = sort xs
check :: ([Float] -> Float -> Bool) -> IO ()
check hasCloseElementsFunc = do
  let testCases =
        [ ([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.3, True),
          ([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.05, False),
          ([1.0, 2.0, 5.9, 4.0, 5.0], 0.95, True),
          ([1.0, 2.0, 5.9, 4.0, 5.0], 0.8, False),
          ([1.0, 2.0, 3.0, 4.0, 5.0, 2.0], 0.1, True),
          ([1.1, 2.2, 3.1, 4.1, 5.1], 1.0, True),
          ([1.1, 2.2, 3.1, 4.1, 5.1], 0.5, False)
        ]

  mapM_ (\(numbers, threshold, expected) -> testCase numbers threshold expected) testCases
  where
    testCase numbers threshold expected = do
      let result = hasCloseElementsFunc numbers threshold
      putStrLn $
        "Input: " ++ show numbers ++ ", Threshold: " ++ show threshold ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."


main :: IO ()
main = check hasCloseElements