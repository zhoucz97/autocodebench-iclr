import Data.Maybe (listToMaybe)

-- Helper function to check if a number is prime
isPrime :: Int -> Bool
isPrime n
    | n <= 1    = False
    | n == 2    = True
    | even n    = False
    | otherwise = null [x | x <- [3,5..floor (sqrt (fromIntegral n))], n `mod` x == 0]

-- Function to find three primes that sum to the given odd number
goldbachConjecture :: Int -> Maybe (Int, Int, Int)
goldbachConjecture n
    | n <= 9 || even n = Nothing
    | otherwise = listToMaybe [(p1, p2, p3) | 
        p1 <- primesUpTo n,
        p2 <- primesUpTo (n - p1),
        let p3 = n - p1 - p2,
        isPrime p3]
    where
        primesUpTo m = filter isPrime [2..m]
check :: (Int -> Maybe (Int, Int, Int)) -> IO ()
check goldbachConjectureFunc = do
  let testCases =
        [ (27, Just (2, 2, 23)),
          (31, Just (3, 5, 23)),
          (45, Just (2, 2, 41)),
          (29, Just (3, 3, 23)),
          (53, Just (3, 3, 47))
        ]

  mapM_ (\(n, expected) -> testCase n expected) testCases
  where
    testCase n expected = do
      let result = goldbachConjectureFunc n
      putStrLn $
        "Input: " ++ show n ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else " (Fail)"

main :: IO ()
main = check goldbachConjecture