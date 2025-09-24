signOfNumber :: Int -> String
signOfNumber n
    | n > 0     = "positive"
    | n < 0     = "negative"
    | otherwise = "zero"
check :: (Int -> String) -> IO ()
check signOfNumberFunc = do
  let testCases = 
        [ (95, "positive"),
          (-5, "negative"),
          (0, "zero"),
          (-1000000000, "negative"),
          (1000000000, "positive")
        ]

  mapM_ (\(number, expected) -> testCase number expected) testCases
  where
    testCase number expected = do
      let result = signOfNumberFunc number
      putStrLn $
        "Input: " ++ show number ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check signOfNumber