charToAscii :: Char -> Int
charToAscii c = fromEnum c
check :: (Char -> Int) -> IO ()
check charToAsciiFunc = do
  let testCases = 
        [ ('A', 65),
          ('a', 97),
          ('0', 48),
          ('$', 36),
          ('9', 57)
        ]

  mapM_ (\(char, expected) -> testCase char expected) testCases
  where
    testCase char expected = do
      let result = charToAsciiFunc char
      putStrLn $
        "Input: " ++ show char ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check charToAscii