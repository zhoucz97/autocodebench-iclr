calculateExpression :: Int -> Int -> Int -> Int
calculateExpression a b c = (a + b) * c
check :: (Int -> Int -> Int -> Int) -> IO ()
check calculateExpressionFunc = do
  let testCases = 
        [ ((2, 3, 5), 25),
          ((-1, 1, 10), 0),
          ((10, 20, -3), -90),
          ((0, 0, 100), 0),
          ((-100, 100, 0), 0)
        ]

  mapM_ (\((a, b, c), expected) -> testCase a b c expected) testCases
  where
    testCase a b c expected = do
      let result = calculateExpressionFunc a b c
      putStrLn $
        "Input: a=" ++ show a ++ ", b=" ++ show b ++ ", c=" ++ show c ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check calculateExpression