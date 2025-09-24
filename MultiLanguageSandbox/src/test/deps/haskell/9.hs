arrayStringsAreEqual :: [String] -> [String] -> Bool
arrayStringsAreEqual xs ys = concat xs == concat ys
check :: ([String] -> [String] -> Bool) -> IO ()
check arrayStringsAreEqualFunc = do
  let testCases =
        [ (["ab", "c"], ["a", "bc"], True),
          (["a", "cb"], ["ab", "c"], False),
          (["abc", "d", "defg"], ["abcddefg"], True),
          (["hello", "world"], ["hello", "planet"], False),
          (["", ""], ["", ""], True)
        ]

  mapM_ (\(word1, word2, expected) -> testCase word1 word2 expected) testCases
  where
    testCase word1 word2 expected = do
      let result = arrayStringsAreEqualFunc word1 word2
      putStrLn $
        "Input: word1 = " ++ show word1 ++ ", word2 = " ++ show word2 ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check arrayStringsAreEqual