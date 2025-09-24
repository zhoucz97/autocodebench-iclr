import Data.List (sort, sortBy)
import qualified Data.Map as Map

frequencySort :: [Int] -> [Int]
frequencySort xs = concatMap (\(v, cnt) -> replicate cnt v) $ 
                   sortBy compareFreq $ 
                   Map.toList $ 
                   foldr (\x -> Map.insertWith (+) x 1) Map.empty xs
  where
    compareFreq (v1, cnt1) (v2, cnt2)
      | cnt1 < cnt2   = LT
      | cnt1 > cnt2   = GT
      | otherwise     = compare v2 v1  -- descending for values with same frequency
check :: ([Int] -> [Int]) -> IO ()
check frequencySortFunc = do
  let testCases =
        [ ([1,1,2,2,2,3], [3,1,1,2,2,2]),
          ([2,3,1,3,2], [1,3,3,2,2]),
          ([5,5,4,6,4], [6,5,5,4,4]),
          ([-1,1,-6,4,5,-6,1,4,1], [5,-1,4,4,-6,-6,1,1,1]),
          ([1], [1])
        ]

  mapM_ (\(nums, expected) -> testCase nums expected) testCases
  where
    testCase nums expected = do
      let result = frequencySortFunc nums
      putStrLn $
        "Input: " ++ show nums ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check frequencySort