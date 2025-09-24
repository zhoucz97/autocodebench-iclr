highestAltitude :: [Int] -> Int
highestAltitude gains = maximum (0 : scanl (+) 0 gains)
check :: ([Int] -> Int) -> IO ()
check highestAltitudeFunc = do
  let testCases =
        [ ([-5,1,5,0,-7], 1),
          ([-4,-3,-2,-1,4,3,2], 0),
          ([44,32,-9,34,-7,-6,-5,35,37,-6], 155),
          ([0,0,0,0], 0),
          ([1,2,3,4], 10)
        ]

  mapM_ (\(gain, expected) -> testCase gain expected) testCases
  where
    testCase gain expected = do
      let result = highestAltitudeFunc gain
      putStrLn $
        "Input: gain = " ++ show gain ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check highestAltitude