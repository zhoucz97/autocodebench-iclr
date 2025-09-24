multiply :: Int -> Int -> Int
multiply a b = a * b
check :: (Int -> Int -> Int) -> IO ()
check multiplyFunc = do
  let testCases = 
        [ ((3, 4), 12),
          ((36, 18), 648),
          ((1, 50000), 50000),
          ((123, 456), 56088),
          ((50000, 50000), 2500000000)
        ]

  mapM_ (\((a, b), expected) -> testCase a b expected) testCases
  where
    testCase a b expected = do
      let result = multiplyFunc a b
      putStrLn $
        "Input: " ++ show a ++ " * " ++ show b ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check multiply