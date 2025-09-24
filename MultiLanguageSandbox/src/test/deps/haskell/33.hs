configureMethod :: Int -> String
configureMethod n
    | localTime <= luoguTime = "Local"
    | otherwise = "Luogu"
    where
        localTime = 5 * n
        luoguTime = 11 + 3 * n
check :: (Int -> String) -> IO ()
check configureMethodFunc = do
  let testCases = 
        [ (1, "Local"),
          (4, "Local"),
          (10, "Luogu"),
          (3, "Local"),
          (20, "Luogu")
        ]

  mapM_ (\(problems, expected) -> testCase problems expected) testCases
  where
    testCase problems expected = do
      let result = configureMethodFunc problems
      putStrLn $
        "Input: " ++ show problems ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check configureMethod