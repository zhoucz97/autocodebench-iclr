sumOddLengthSubarrays :: [Int] -> Int
sumOddLengthSubarrays arr = sum [sum (take l (drop i arr)) | l <- [1,3..n], i <- [0..n-l]]
  where n = length arr
check :: ([Int] -> Int) -> IO ()
check sumOddLengthSubarraysFunc = do
  let testCases =
        [ ([1,4,2,5,3], 58),
          ([1,2], 3),
          ([10,11,12], 66),
          ([1], 1)
        ]

  mapM_ (\(arr, expected) -> testCase arr expected) testCases
  where
    testCase arr expected = do
      let result = sumOddLengthSubarraysFunc arr
      putStrLn $
        "Input: " ++ show arr ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check sumOddLengthSubarrays