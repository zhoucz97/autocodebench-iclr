absoluteValue :: Int -> Int
absoluteValue x
    | abs x > 10000 = error "Input absolute value exceeds 10000"
    | otherwise     = abs x
check :: (Int -> Int) -> IO ()
check absoluteValueFunc = do
  let testCases = 
        [ (-3, 3),
          (5, 5),
          (0, 0),
          (-10000, 10000),
          (9999, 9999)
        ]

  mapM_ (\(number, expected) -> testCase number expected) testCases
  where
    testCase number expected = do
      let result = absoluteValueFunc number
      putStrLn $
        "Input: " ++ show number ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check absoluteValue