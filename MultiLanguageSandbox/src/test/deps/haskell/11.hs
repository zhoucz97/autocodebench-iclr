import Data.List (all)

countConsistentStrings :: String -> [String] -> Int
countConsistentStrings allowed words = length $ filter (isConsistent allowed) words
  where
    isConsistent :: String -> String -> Bool
    isConsistent allowed word = all (`elem` allowed) word
check :: (String -> [String] -> Int) -> IO ()
check countConsistentStringsFunc = do
  let testCases =
        [ ("ab", ["ad","bd","aaab","baa","badab"], 2),
          ("abc", ["a","b","c","ab","ac","bc","abc","abcd","abdc"], 7),
          ("cad", ["cc","acd","b","ba","bac","bad","ac","d"], 4),
          ("z", ["z","y","zzz","zyz"], 2)
        ]

  mapM_ (\(allowed, words, expected) -> testCase allowed words expected) testCases
  where
    testCase allowed words expected = do
      let result = countConsistentStringsFunc allowed words
      putStrLn $
        "Input: allowed = " ++ show allowed ++ ", words = " ++ show words ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check countConsistentStrings