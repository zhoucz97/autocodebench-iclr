import qualified Data.Set as Set

countMazePaths :: Int -> Int -> Int -> (Int, Int) -> (Int, Int) -> Set.Set (Int, Int) -> Int
countMazePaths n m _ start end obstacles = dfs start Set.empty
  where
    dfs :: (Int, Int) -> Set.Set (Int, Int) -> Int
    dfs (x, y) visited
      | (x, y) == end = 1
      | x < 1 || x > n || y < 1 || y > m = 0
      | Set.member (x, y) obstacles = 0
      | Set.member (x, y) visited = 0
      | otherwise =
          let newVisited = Set.insert (x, y) visited
              up = dfs (x - 1, y) newVisited
              down = dfs (x + 1, y) newVisited
              left = dfs (x, y - 1) newVisited
              right = dfs (x, y + 1) newVisited
          in up + down + left + right
check :: (Int -> Int -> Int -> (Int, Int) -> (Int, Int) -> Set.Set (Int, Int) -> Int) -> IO ()
check countMazePathsFunc = do
  let testCases =
        [ ((3, 3, 1, (1, 1), (3, 3), Set.fromList [(2, 2)]), 2),
          ((4, 4, 2, (1, 1), (4, 4), Set.fromList [(2, 2), (3, 3)]), 4),
          ((2, 3, 0, (1, 1), (2, 3), Set.empty), 4)
        ]

  mapM_ (\(params, expected) -> testCase params expected) testCases
  where
    testCase (n, m, t, start, end, obstacles) expected = do
      let result = countMazePathsFunc n m t start end obstacles
      putStrLn $
        "Input: " ++ show (n, m, t, start, end, obstacles) ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check countMazePaths