decrypt :: [Int] -> Int -> [Int]
decrypt xs k
  | k == 0    = replicate (length xs) 0
  | k == len  = replicate len total
  | otherwise = map (\i -> sum $ take k $ drop (i + 1) $ cycle xs) [0..len - 1]
  where
    len = length xs
    total = sum xs
check :: ([Int] -> Int -> [Int]) -> IO ()
check decryptFunc = do
  let testCases =
        [ ([5,7,1,4], 3, [12,10,16,13]),
          ([1,2,3,4], 0, [0,0,0,0]),
          ([2,4,9,3], -2, [12,5,6,13]),
          ([10,5,7,7], 1, [5,7,7,10]),
          ([1,2,3,4], 4, [10,10,10,10])
        ]

  mapM_ (\(code, k, expected) -> testCase code k expected) testCases
  where
    testCase code k expected = do
      let result = decryptFunc code k
      putStrLn $
        "Input: code = " ++ show code ++ ", k = " ++ show k ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check decrypt