import Data.Char (isDigit)
import Data.Maybe (catMaybes)

-- --- Day 1: Trebuchet?! ---
-- Something is wrong with global snow production, and you've been selected to take a look.
-- The Elves have even given you a map; on it, they've used stars to mark the top fifty locations that are likely to be having problems.

-- You've been doing this long enough to know that to restore snow operations, you need to check all fifty stars by December 25th.

-- Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first.
-- Each puzzle grants one star. Good luck!

-- You try to ask why they can't just use a weather machine ("not powerful enough") and where
-- they're even sending you ("the sky") and why your map looks mostly blank ("you sure ask a lot of questions")
-- and hang on did you just say the sky ("of course, where do you think snow comes from") when
-- you realize that the Elves are already loading you into a trebuchet ("please hold still, we need to strap you in").

-- As they're making the final adjustments, they discover that their calibration document (your puzzle input) has
-- been amended by a very young Elf who was apparently just excited to show off her art skills.

-- Consequently, the Elves are having trouble reading the values on the document.

-- The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration value
-- that the Elves now need to recover.
-- On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.

-- For example:

-- 1abc2
-- pqr3stu8vwx
-- a1b2c3d4e5f
-- treb7uchet
-- In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.

-- Consider your entire calibration document. What is the sum of all of the calibration values?

-- Your puzzle answer was 54708.

-- The first half of this puzzle is complete! It provides one gold star: *

-- --- Part Two ---
-- Your calculation isn't quite right. It looks like some of the digits are actually spelled out with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".

-- Equipped with this new information, you now need to find the real first and last digit on each line. For example:

-- two1nine
-- eightwothree
-- abcone2threexyz
-- xtwone3four
-- 4nineeightseven2
-- zoneight234
-- 7pqrstsixteen
-- In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.

-- What is the sum of all of the calibration values?

main = interact mainFunc

mainFunc :: String -> String
mainFunc input =
  let allLines = lines input
      firstDigits = [findDigitFrom 0 1 x | x <- allLines]
      lastDigits = [findDigitFrom (length x) (-1) x | x <- allLines]
      multipliedFirsts = map (* 10) firstDigits
      completeLineValues = zipWith (+) multipliedFirsts lastDigits
      theTotal = sum completeLineValues
      result = show theTotal ++ "\n"
   in result

data StringDigitMapper = StringDigitMapper String Int deriving (Show, Eq)

stringDigitMapping =
  [ StringDigitMapper "0" 0,
    StringDigitMapper "1" 1,
    StringDigitMapper "2" 2,
    StringDigitMapper "3" 3,
    StringDigitMapper "4" 4,
    StringDigitMapper "5" 5,
    StringDigitMapper "6" 6,
    StringDigitMapper "7" 7,
    StringDigitMapper "8" 8,
    StringDigitMapper "9" 9,
    StringDigitMapper "one" 1,
    StringDigitMapper "two" 2,
    StringDigitMapper "three" 3,
    StringDigitMapper "four" 4,
    StringDigitMapper "five" 5,
    StringDigitMapper "six" 6,
    StringDigitMapper "seven" 7,
    StringDigitMapper "eight" 8,
    StringDigitMapper "nine" 9
  ]

findDigitFrom :: Int -> Int -> String -> Int
findDigitFrom position positionIncrement theString =
  let workingString = drop position theString
      digitMatches = [if matchDigit x workingString then Just y else Nothing | (StringDigitMapper x y) <- stringDigitMapping]
      actualMatches = catMaybes digitMatches
   in if not (null actualMatches) then head actualMatches else findDigitFrom (position + positionIncrement) positionIncrement theString

-- short string match, there's probably a smarter way to do this..
matchDigit :: String -> String -> Bool
matchDigit a b =
  let chopped = take (length a) b
   in chopped == a
