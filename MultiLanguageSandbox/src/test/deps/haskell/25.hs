misplacedLetters :: Int -> Int
misplacedLetters 0 = 1
misplacedLetters 1 = 0
misplacedLetters n = (n - 1) * (misplacedLetters (n - 1) + misplacedLetters (n - 2))
check :: (Int -> Int) -> IO ()
check misplacedLettersFunc = do
  let testCases =
        [ (1, 0),
          (4, 9),
          (5, 44),
          (6, 265)
        ]

  mapM_ (\(letters, expected) -> testCase letters expected) testCases
  where
    testCase letters expected = do
      let result = misplacedLettersFunc letters
      putStrLn $
        "Input: " ++ show letters ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check misplacedLetters