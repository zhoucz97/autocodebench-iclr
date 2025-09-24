diagonalSum :: [[Int]] -> Int
diagonalSum matrix = sum mainDiag + sum secondaryDiag - intersect
  where
    n = length matrix
    mainDiag = [matrix !! i !! i | i <- [0..n-1]]
    secondaryDiag = [matrix !! i !! (n - 1 - i) | i <- [0..n-1]]
    intersect = if odd n then matrix !! mid !! mid else 0
    mid = n `div` 2
check :: ([[Int]] -> Int) -> IO ()
check diagonalSumFunc = do
  let testCases =
        [ ([[1,2,3], [4,5,6], [7,8,9]], 25),
          ([[1,1], [1,1]], 4),
          ([[5]], 5),
          ([[1,2], [3,4]], 10),
          ([[2,0,0], [0,1,0], [0,0,3]], 6)
        ]

  mapM_ (\(mat, expected) -> testCase mat expected) testCases
  where
    testCase mat expected = do
      let result = diagonalSumFunc mat
      putStrLn $
        "Input: " ++ show mat ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check diagonalSum