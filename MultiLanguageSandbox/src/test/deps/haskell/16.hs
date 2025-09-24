import Data.List (sort)

isRotationPossible :: [Int] -> Bool
isRotationPossible nums = 
    let sorted = sort nums
        doubled = sorted ++ sorted
    in nums `elem` tails doubled
check :: ([Int] -> Bool) -> IO ()
check isRotationPossibleFunc = do
  let testCases =
        [ ([3,4,5,1,2], True),
          ([2,1,3,4], False),
          ([1,2,3], True),
          ([1,1,1,2,2], True),
          ([2,3,4,5,1,1], True)
        ]

  mapM_ (\(nums, expected) -> testCase nums expected) testCases
  where
    testCase nums expected = do
      let result = isRotationPossibleFunc nums
      putStrLn $
        "Input: nums = " ++ show nums ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check isRotationPossible