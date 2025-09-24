maxRemovableLamps :: Int -> Int -> [Int] -> Int
maxRemovableLamps dist n lamps = go 1 (lamps !! 1) (lamps !! 2) (tail (tail lamps)) 1
  where
    go :: Int -> Int -> Int -> [Int] -> Int -> Int
    go i prev current rest count
      | null rest = count
      | otherwise =
          let next = head rest
              canRemove = abs (current - prev) <= dist && abs (next - current) <= dist
          in if canRemove
             then go (i+1) current next (tail rest) (count + 1)
             else go (i+1) current next (tail rest) count
check :: (Int -> Int -> [Int] -> Int) -> IO ()
check maxRemovableLampsFunc = do
  let testCases =
        [ (3, 3, [1, 2, 3], 1),
          (5, 2, [1, 3, 4, 6, 8], 0),
          (4, 5, [2, 5, 7, 12], 1),
          (6, 4, [3, 5, 8, 12, 16, 20], 0),
          (3, 10, [5, 15, 25], 0)
        ]

  mapM_ (\(n, dist, positions, expected) -> testCase n dist positions expected) testCases
  where
    testCase n dist positions expected = do
      let result = maxRemovableLampsFunc n dist positions
      putStrLn $
        "Input: n=" ++ show n ++ ", dist=" ++ show dist ++ ", positions=" ++ show positions ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check maxRemovableLamps