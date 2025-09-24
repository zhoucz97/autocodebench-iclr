closestIndex :: [Int] -> Int -> Int -> Int
closestIndex nums target start = 
    let indices = [i | (i, x) <- zip [0..] nums, x == target]
        distances = map (\i -> abs (i - start)) indices
        minDistance = minimum distances
    in head $ filter (\i -> abs (i - start) == minDistance) indices
check :: ([Int] -> Int -> Int -> Int) -> IO ()
check closestIndexFunc = do
  let testCases =
        [ ([1,2,3,4,5], 5, 3, 4),
          ([1], 1, 0, 0),
          ([1,2,3,4,5,6,7,8,9,10], 5, 5, 4),
          ([1,2,3,4,3,5], 3, 3, 2),
          ([1,2,3,4,5,6,7,8,9,10], 7, 2, 6)
        ]

  mapM_ (\(nums, target, start, expected) -> testCase nums target start expected) testCases
  where
    testCase nums target start expected = do
      let result = closestIndexFunc nums target start
      putStrLn $
        "Input: nums = " ++ show nums ++ ", target = " ++ show target ++ ", start = " ++ show start ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check closestIndex