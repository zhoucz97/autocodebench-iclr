averageWithoutExtremes :: [Int] -> Maybe Float
averageWithoutExtremes xs
    | length xs < 3 = Nothing
    | otherwise = Just (fromIntegral sumWithoutExtremes / fromIntegral (length xs - 2))
    where
        sorted = sort xs
        sumWithoutExtremes = sum (tail (init sorted))
check :: ([Int] -> Maybe Float) -> IO ()
check averageWithoutExtremesFunc = do
  let testCases =
        [ ([1,2,3,4,5], Just 3.0),
          ([1,2], Nothing),
          ([10,20,30,40,50], Just 30.0),
          ([5,5,5,5,5], Just 5.0),
          ([7], Nothing)
        ]

  mapM_ (\(arr, expected) -> testCase arr expected) testCases
  where
    testCase arr expected = do
      let result = averageWithoutExtremesFunc arr
      putStrLn $
        "Input: " ++ show arr ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check averageWithoutExtremes