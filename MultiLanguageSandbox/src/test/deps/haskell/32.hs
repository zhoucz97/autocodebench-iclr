sentenceWithApples :: Int -> String
sentenceWithApples n
    | n == 1    = "Today, I ate 1 apple."
    | n == 0    = "Today, I ate 0 apples."
    | otherwise = "Today, I ate " ++ show n ++ " apples."
check :: (Int -> String) -> IO ()
check sentenceWithApplesFunc = do
  let testCases =
        [ (1, "Today, I ate 1 apple."),
          (0, "Today, I ate 0 apples."),
          (2, "Today, I ate 2 apples."),
          (5, "Today, I ate 5 apples.")
        ]

  mapM_ (\(apples, expected) -> testCase apples expected) testCases
  where
    testCase apples expected = do
      let result = sentenceWithApplesFunc apples
      putStrLn $
        "Input: " ++ show apples ++
        ", Expected: " ++ show expected ++ ", Result: " ++ result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check sentenceWithApples