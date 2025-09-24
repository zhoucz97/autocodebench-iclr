findCommonAwardWinners :: [Int] -> [Int] -> [Int]
findCommonAwardWinners sciAwardees specAwardees = 
    filter (`elem` specAwardees) sciAwardees
check :: ([Int] -> [Int] -> [Int]) -> IO ()
check findCommonAwardWinnersFunc = do
  let testCases =
        [ (([1,2,3,4], [3,5,6]), [3]),
          (([10,20,30], [40,50,60]), []),
          (([7,8,9,10], [10,9,8]), [8,9,10]),
          (([15,25,35,45], [25,35,55,65]), [25,35]),
          (([4,5,6], [6,7,8,9]), [6])
        ]

  mapM_ (\((innovation, special), expected) -> testCase innovation special expected) testCases
  where
    testCase innovation special expected = do
      let result = findCommonAwardWinnersFunc innovation special
      putStrLn $
        "Input: " ++ show (innovation, special) ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check findCommonAwardWinners