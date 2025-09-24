countMatches :: [[String]] -> String -> String -> Int
countMatches items key value = length $ filter (\item -> item !! index == value) items
  where
    index = case key of
        "type"  -> 0
        "color" -> 1
        "name"  -> 2
        _       -> error "Unknown key"
check :: ([[String]] -> String -> String -> Int) -> IO ()
check countMatchesFunc = do
  let testCases =
        [ ([["phone","blue","pixel"],["computer","silver","lenovo"],["phone","gold","iphone"]], "color", "silver", 1),
          ([["phone","blue","pixel"],["computer","silver","phone"],["phone","gold","iphone"]], "type", "phone", 2),
          ([["phone","blue","pixel"],["computer","silver","lenovo"],["phone","gold","iphone"]], "name", "iphone", 1),
          ([["phone","blue","pixel"],["computer","silver","lenovo"],["phone","gold","iphone"]], "color", "blue", 1),
          ([["phone","blue","pixel"],["computer","silver","lenovo"],["phone","gold","iphone"]], "name", "lenovo", 1)
        ]

  mapM_ (\(items, ruleKey, ruleValue, expected) -> testCase items ruleKey ruleValue expected) testCases
  where
    testCase items ruleKey ruleValue expected = do
      let result = countMatchesFunc items ruleKey ruleValue
      putStrLn $
        "Input: items = " ++ show items ++ ", ruleKey = " ++ show ruleKey ++ ", ruleValue = " ++ show ruleValue ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check countMatches