truncateSentence :: String -> Int -> String
truncateSentence s k = unwords $ take k (words s)
check :: (String -> Int -> String) -> IO ()
check truncateSentenceFunc = do
  let testCases =
        [ ("Hello World", 1, "Hello"),
          ("one two three four", 2, "one two"),
          ("What is the solution to this problem", 4, "What is the solution"),
          ("HELLO", 1, "HELLO"),
          ("hello world hello world", 3, "hello world hello")
        ]

  mapM_ (\(s, k, expected) -> testCase s k expected) testCases
  where
    testCase s k expected = do
      let result = truncateSentenceFunc s k
      putStrLn $
        "Input: s = " ++ show s ++ ", k = " ++ show k ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check truncateSentence