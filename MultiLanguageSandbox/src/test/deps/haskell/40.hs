rangeDifference :: [Int] -> Int
rangeDifference [] = 0  -- handle empty list case (though examples don't show this)
rangeDifference xs = maximum xs - minimum xs
check :: ([Int] -> Int) -> IO ()
check rangeDifferenceFunc = do
  let testCases =
        [ ([7, 3, 9, 5], 6),
          ([10, 20, 30, 40, 50], 40),
          ([15], 0),
          ([1, 1, 1, 1], 0),
          ([8, 6, 7, 5, 3, 0, 9], 9)
        ]

  mapM_ (\(ints, expected) -> testCase ints expected) testCases
  where
    testCase ints expected = do
      let result = rangeDifferenceFunc ints
      putStrLn $
        "Input: " ++ show ints ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check rangeDifference