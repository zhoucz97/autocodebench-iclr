countMaxLenSquares :: [[Int]] -> Int
countMaxLenSquares rectangles = length $ filter (\x -> min (head x) (x !! 1) == maxMin) rectangles
  where
    mins = map (\x -> min (head x) (x !! 1)) rectangles
    maxMin = maximum mins
check :: ([[Int]] -> Int) -> IO ()
check countMaxLenSquaresFunc = do
  let testCases =
        [ ([[5,8],[3,9],[5,12],[16,5]], 3),
          ([[2,3],[3,7],[4,3],[3,7]], 3),
          ([[2,2],[3,3],[4,4],[5,5]], 1),
          ([[2,5],[3,3],[5,8],[6,10]], 1),
          ([[1,1],[2,2],[3,3],[4,1],[2,2],[1,4]], 1)
        ]

  mapM_ (\(rectangles, expected) -> testCase rectangles expected) testCases
  where
    testCase rectangles expected = do
      let result = countMaxLenSquaresFunc rectangles
      putStrLn $
        "Input: rectangles = " ++ show rectangles ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check countMaxLenSquares