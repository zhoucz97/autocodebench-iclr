import Data.List (group, sort)

sumOfUnique :: [Int] -> Int
sumOfUnique xs = sum [x | (x:xs') <- group (sort xs), length (x:xs') == 1]
check :: ([Int] -> Int) -> IO ()
check sumOfUniqueFunc = do
  let testCases =
        [ ([1,2,3,2], 4),
          ([1,1,1,1,1], 0),
          ([1,2,3,4,5], 15),
          ([], 0),
          ([7,7,7,7,7,8,8], 0)
        ]

  mapM_ (\(nums, expected) -> testCase nums expected) testCases
  where
    testCase nums expected = do
      let result = sumOfUniqueFunc nums
      putStrLn $
        "Input: nums = " ++ show nums ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check sumOfUnique