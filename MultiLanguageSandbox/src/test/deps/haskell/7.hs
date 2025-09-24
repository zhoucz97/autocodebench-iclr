import Data.List (find)

canFormArray :: [Int] -> [[Int]] -> Bool
canFormArray arr pieces = go arr pieces
  where
    go [] _ = True
    go _ [] = False
    go (x:xs) ps =
      case find (\p -> not (null p) && head p == x) ps of
        Nothing -> False
        Just p ->
          let (matched, remaining) = splitAt (length p) (x:xs)
          in if matched == p
             then go (drop (length p) (x:xs)) (filter (/= p) ps)
             else False
check :: ([Int] -> [[Int]] -> Bool) -> IO ()
check canFormArrayFunc = do
  let testCases =
        [ ([85], [[85]], True),
          ([15,88], [[88],[15]], True),
          ([49,18,16], [[16,18,49]], False),
          ([91,4,64,78], [[78],[4,64],[91]], True),
          ([1,3,5,7], [[2,4,6,8]], False)
        ]

  mapM_ (\(arr, pieces, expected) -> testCase arr pieces expected) testCases
  where
    testCase arr pieces expected = do
      let result = canFormArrayFunc arr pieces
      putStrLn $
        "Input: arr = " ++ show arr ++ ", pieces = " ++ show pieces ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check canFormArray