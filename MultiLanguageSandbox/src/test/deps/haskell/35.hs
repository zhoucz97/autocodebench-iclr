isLeapYear :: Int -> Bool
isLeapYear year
    | year `mod` 400 == 0 = True
    | year `mod` 100 == 0 = False
    | year `mod` 4 == 0   = True
    | otherwise           = False

daysInMonth :: Int -> Int -> Int
daysInMonth year month
    | month == 2  = if isLeapYear year then 29 else 28
    | month `elem` [4, 6, 9, 11] = 30
    | otherwise                 = 31
check :: (Int -> Int -> Int) -> IO ()
check daysInMonthFunc = do
  let testCases =
        [ ((2021, 2), 28),
          ((2024, 2), 29),
          ((2021, 4), 30),
          ((2021, 12), 31),
          ((1900, 2), 28),
          ((2000, 2), 29)
        ]

  mapM_ (\((year, month), expected) -> testCase year month expected) testCases
  where
    testCase year month expected = do
      let result = daysInMonthFunc year month
      putStrLn $
        "Input: Year " ++ show year ++ " Month " ++ show month ++
        ", Expected: " ++ show expected ++ ", Result: " ++ show result ++
        if result == expected then " (Pass)" else error "Test failed. Exiting..."

main :: IO ()
main = check daysInMonth