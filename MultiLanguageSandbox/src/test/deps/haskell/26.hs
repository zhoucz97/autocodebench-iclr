import Data.Char (digitToInt, intToDigit)

baseAddition :: Int -> String -> String -> String
baseAddition base num1 num2 = 
    let n1 = stringToBase base num1
        n2 = stringToBase base num2
        sumN = n1 + n2
    in baseToString base sumN

-- Helper function to convert a string in a given base to an integer
stringToBase :: Int -> String -> Int
stringToBase base str = foldl (\acc c -> acc * base + digitToInt c) 0 str

-- Helper function to convert an integer to a string in a given base
baseToString :: Int -> Int -> String
baseToString base 0 = "0"
baseToString base n = 
    let digits = reverse $ go n
    in if null digits then "0" else digits
    where
        go 0 = []
        go m = intToDigit (m `mod` base) : go (m `div` base)
check :: (Int -> String -> String -> String) -> IO ()
check baseAdditionFunc = do
  let testCases =
        [ ((4, "123", "321"), "1110"),
          ((2, "1010", "1101"), "10111"),
          ((16, "1A3", "2B4"), "457"),
          ((10, "123", "456"), "579"),
          ((8, "123", "456"), "601")
        ]

  mapM_ (\((base, num1, num2), expected) -> testCase base num1 num2 expected) testCases
  where
    testCase base num1 num2 expected = do
      let result = baseAdditionFunc base num1 num2
      putStrLn $
        "Base: " ++ show base ++
        ", Input 1: " ++ num1 ++
        ", Input 2: " ++ num2 ++
        ", Expected: " ++ expected ++ ", Result: " ++ result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check baseAddition