countUnsatisfiedStudents :: [Int] -> [Int] -> Int
countUnsatisfiedStudents students sandwiches = length unsatisfied
  where
    (satisfied, unsatisfied) = foldl step ([], students) sandwiches
    step (sat, []) _ = (sat, [])
    step (sat, (s:ss)) sw
      | s == sw    = (sat ++ [s], ss)
      | otherwise  = (sat, ss ++ [s])
check :: ([Int] -> [Int] -> Int) -> IO ()
check countUnsatisfiedStudentsFunc = do
  let testCases =
        [ ([1,1,0,0], [0,1,0,1], 0),
          ([1,1,1,0,0,1], [1,0,0,0,1,1], 3),
          ([1,0,0,1,1], [1,1,0,0,0], 1),
          ([0,0,1,1], [0,1,0,1], 0)
        ]

  mapM_ (\(students, sandwiches, expected) -> testCase students sandwiches expected) testCases
  where
    testCase students sandwiches expected = do
      let result = countUnsatisfiedStudentsFunc students sandwiches
      putStrLn $
        "Input: students = " ++ show students ++ ", sandwiches = " ++ show sandwiches ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check countUnsatisfiedStudents