maximumWealth :: [[Int]] -> Int
maximumWealth = maximum . map sum
check :: ([[Int]] -> Int) -> IO ()
check maximumWealthFunc = do
  let testCases =
        [ ([[1,2,3],[3,2,1]], 6),
          ([[1,5],[7,3],[3,5]], 10),
          ([[2,8,7],[7,1,3],[1,9,5]], 17),
          ([[10],[15],[20]], 20),
          ([[1,2],[3,4],[5,6,7]], 18)
        ]

  mapM_ (\(accounts, expected) -> testCase accounts expected) testCases
  where
    testCase accounts expected = do
      let result = maximumWealthFunc accounts
      putStrLn $
        "Input: " ++ show accounts ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check maximumWealth