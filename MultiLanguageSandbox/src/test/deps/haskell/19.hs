signOfProduct :: [Int] -> Int
signOfProduct xs = signFunc (product xs)
  where
    signFunc x
      | x > 0     = 1
      | x < 0     = 1
      | otherwise = 0
check :: ([Int] -> Int) -> IO ()
check signOfProductFunc = do
  let testCases =
        [ ([1,-2,3,4,5], -1),
          ([1,2,3,4,5], 1),
          ([-1,-2,-3,-4,-5], -1),
          ([1,5,0,2,-3], 0),
          ([], 1)
        ]

  mapM_ (\(nums, expected) -> testCase nums expected) testCases
  where
    testCase nums expected = do
      let result = signOfProductFunc nums
      putStrLn $
        "Input: nums = " ++ show nums ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check signOfProduct