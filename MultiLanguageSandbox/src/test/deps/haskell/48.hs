asciiToChar :: Int -> Char
asciiToChar = chr
check :: (Int -> Char) -> IO ()
check asciiToCharFunc = do
  let testCases = 
        [ (65, 'A'),
          (97, 'a'),
          (48, '0'),
          (36, '$'),
          (57, '9')
        ]

  mapM_ (\(code, expected) -> testCase code expected) testCases
  where
    testCase code expected = do
      let result = asciiToCharFunc code
      putStrLn $
        "Input: " ++ show code ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check asciiToChar