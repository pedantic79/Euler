isLeapYear y | y == 1900      = False
             | y `mod` 4 == 0 = True
             | otherwise      = False

getCalendar y = [ 31, feb, 31, 30, 31, 30
                , 31, 31,  30, 31, 30, 31
                ]
  where feb = if isLeapYear y then 29 else 28

-- day of the week for Jan 1
firstDayYear 1900 = 0
firstDayYear y = last (firstDayMons (y - 1)) `mod` 7

-- Contains the first day of every month
-- last element is the first day of the previous month
firstDayMons y = scanl (+) (firstDayYear y) (getCalendar y)


problem19 = length . filter (==6) . concatMap getDaysOfWeek $ [1901..2000]
  where getDaysOfWeek = init . map (`mod` 7) . firstDayMons

