findMinimum :: [Int] -> Int
findMinimum [] = error "Empty list has no minimum"
findMinimum (x:xs) = foldl (\acc y -> if y < acc then y else acc) x xs
check :: ([Int] -> Int) -> IO ()
check findMinimumFunc = do
  let testCases =
        [ ([10, 20, 5, 8], 5),
          ([4, 3, 2, 1], 1),
          ([15, 22, 8, 9, 6], 6),
          ([33, 17, 45, 19], 17),
          ([7, 7, 7, 7, 7], 7)
        ]

  mapM_ (\(nums, expected) -> testCase nums expected) testCases
  where
    testCase nums expected = do
      let result = findMinimumFunc nums
      putStrLn $
        "Input: " ++ show nums ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check findMinimum