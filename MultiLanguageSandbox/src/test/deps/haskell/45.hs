nthTerm :: Int -> Int -> Int -> Int
nthTerm a1 a2 n = a1 + (n - 1) * d
  where d = a2 - a1
check :: (Int -> Int -> Int -> Int) -> IO ()
check nthTermFunc = do
  let testCases = 
        [ ((1, 4, 100), 298),
          ((5, 7, 10), 23),
          ((-3, 1, 5), 13),
          ((0, 2, 50), 98)
        ]

  mapM_ (\((a1, a2, n), expected) -> testCase a1 a2 n expected) testCases
  where
    testCase a1 a2 n expected = do
      let result = nthTermFunc a1 a2 n
      putStrLn $
        "Input: a1=" ++ show a1 ++ ", a2=" ++ show a2 ++ ", n=" ++ show n ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check nthTerm