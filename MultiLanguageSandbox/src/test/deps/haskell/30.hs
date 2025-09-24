import Data.List (sort, nub)

findNumberTriplets :: (Int, Int, Int) -> [(Int, Int, Int)]
findNumberTriplets (a, b, c) = 
    let ratios = (a, b, c)
        -- Generate all possible 3-digit numbers
        nums = [100..999]
        -- For each number A, compute B and C based on the ratio, then check conditions
        triplets = [(aNum, bNum, cNum) | 
            aNum <- nums,
            let bNum = (b * aNum) `div` a,
            let cNum = (c * aNum) `div` a,
            bNum >= 100 && bNum <= 999,
            cNum >= 100 && cNum <= 999,
            let combined = show aNum ++ show bNum ++ show cNum,
            length combined == 9,
            sort combined == "123456789"]
    in nub triplets
check :: ((Int, Int, Int) -> [(Int, Int, Int)]) -> IO ()
check findNumberTripletsFunc = do
  let testCases =
        [ ((1, 2, 3), [(192, 384, 576), (219, 438, 657), (273, 546, 819), (327, 654, 981)]),
          ((2, 5, 7), []),
          ((3, 3, 3), [])
        ]

  mapM_ (\(ratios, expected) -> testCase ratios expected) testCases
  where
    testCase ratios expected = do
      let result = findNumberTripletsFunc ratios
      putStrLn $
        "Input: " ++ show ratios ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if sort result == sort expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check findNumberTriplets