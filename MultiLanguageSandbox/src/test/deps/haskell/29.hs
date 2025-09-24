maxHerbValue :: Int -> [(Int, Int)] -> Int
maxHerbValue time herbs = dp !! time
  where
    dp = array (0, time) [(t, maxVal t) | t <- [0..time]]
    maxVal 0 = 0
    maxVal t = maximum [if cost <= t then dp ! (t - cost) + value else 0 | (cost, value) <- herbs]
check :: (Int -> [(Int, Int)] -> Int) -> IO ()
check maxHerbValueFunc = do
  let testCases =
        [ ((70, [(71, 100), (69, 1), (1, 2)]), 140),
          ((50, [(10, 60), (20, 100), (30, 120)]), 300),
          ((10, [(5, 50), (3, 20), (2, 14)]), 100),
          ((0, [(1, 10), (2, 20)]), 0),
          ((15, [(1, 10), (2, 5), (3, 8)]), 150)
        ]

  mapM_ (\((t, herbs), expected) -> testCase t herbs expected) testCases
  where
    testCase t herbs expected = do
      let result = maxHerbValueFunc t herbs
      putStrLn $
        "Input: Time=" ++ show t ++ ", Herbs=" ++ show herbs ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check maxHerbValue