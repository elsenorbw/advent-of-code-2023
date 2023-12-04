import Data.Char (isDigit)
import Data.Map (Map, empty, insert, member)
import Data.Maybe (catMaybes)

-- --- Day 3: Gear Ratios ---
-- You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up to the water source,
-- but this is as far as he can bring you. You go inside.

-- It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.

-- "Aaah!"

-- You turn around to see a slightly-greasy Elf with a wrench and a look of surprise.
-- "Sorry, I wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I can fix it."
-- You offer to help.

-- The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one.
-- If you can add up all the part numbers in the engine schematic, it should be easy to work out which part is missing.

-- The engine schematic (your puzzle input) consists of a visual representation of the engine.
-- There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol,
-- even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)

-- Here is an example engine schematic:

-- 467..114..
-- ...*......
-- ..35..633.
-- ......#...
-- 617*......
-- .....+.58.
-- ..592.....
-- ......755.
-- ...$.*....
-- .664.598..
-- In this schematic, two numbers are not part numbers because they are not adjacent to a symbol:
-- 114 (top right) and 58 (middle right).
-- Every other number is adjacent to a symbol and so is a part number; their sum is 4361.

-- Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?

main = interact mainFunc

mainFunc :: String -> String
mainFunc input =
  let (symbols, partNumbers) = loadEngineRepresentation $ lines input
      partsWithSymbols = findPartNumbersWithSymbols symbols partNumbers
      sumOfParts = sum partsWithSymbols
   in show symbols ++ "\n" ++ show partNumbers ++ "\n" ++ show partsWithSymbols ++ "\n" ++ "Sum of all parts with symbols is " ++ show sumOfParts ++ "\n"

data Location = Location
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Ord)

findPartNumbersWithSymbols :: Map Location Char -> [(Location, String)] -> [Int]
findPartNumbersWithSymbols symbolMap numberList =
  let surroundingLocations = [generateSurroundingLocations numLoc (length s) | (numLoc, s) <- numberList]
      matchedLocations = [checkMatches symbolMap theLocations | theLocations <- surroundingLocations]
      intPartNumbers = [read x :: Int | (_, x) <- numberList]
      usefulPartNumbers = [if hasSymbol then Just intVal else Nothing | (hasSymbol, intVal) <- zip matchedLocations intPartNumbers]
      finalPartList = catMaybes usefulPartNumbers
   in finalPartList

checkMatches :: Map Location Char -> [Location] -> Bool
checkMatches symbolMap locationList =
  let matches = [member l symbolMap | l <- locationList]
      anyMatches = foldl (||) False matches
   in anyMatches

generateSurroundingLocations :: Location -> Int -> [Location]
generateSurroundingLocations l len =
  let xPos = x l
      yPos = y l
      xMin = xPos - 1
      xMax = xPos + len
      left = Location xMin yPos
      right = Location xMax yPos
      aboveY = (yPos - 1)
      belowY = (yPos + 1)
      topRow = [Location xVal aboveY | xVal <- [xMin .. xMax]]
      bottomRow = [Location xVal belowY | xVal <- [xMin .. xMax]]
   in left : right : topRow ++ bottomRow

loadEngineRepresentation :: [String] -> (Map Location Char, [(Location, String)])
loadEngineRepresentation lines =
  let startingMap = Data.Map.empty
      lineSymbols = [readLocations theLine y | (theLine, y) <- zip lines [0 ..]]
      allSymbols = concat lineSymbols
      finalSymbolMap = foldl addToMap startingMap allSymbols
      lineNumbers = [readNumbers theLine 0 y | (theLine, y) <- zip lines [0 ..]]
      finalNumberList = [(Location x y, s) | (x, y, s) <- concat lineNumbers]
   in (finalSymbolMap, finalNumberList)

addToMap :: (Ord a) => Map Location a -> (Int, Int, a) -> Map Location a
addToMap m (xVal, yVal, theSymbol) = insert (Location xVal yVal) theSymbol m

readNumbers :: String -> Int -> Int -> [(Int, Int, String)]
readNumbers "" _ _ = []
readNumbers (s : ss) xValue yValue =
  if isDigit s
    then
      let nextDigits = takeWhile isDigit ss
          remaining = dropWhile isDigit ss
          thisValueString = s : nextDigits
          thisLocation = Location
          nextX = xValue + length thisValueString
       in (xValue, yValue, thisValueString) : readNumbers remaining nextX yValue
    else readNumbers ss (xValue + 1) yValue

readLocations :: String -> Int -> [(Int, Int, Char)]
readLocations s yValue =
  let charsAtLocations = zip s [0 ..]
      allLocations = [if isSymbol x then Just (pos, yValue, x) else Nothing | (x, pos) <- charsAtLocations]
      result = catMaybes allLocations
   in result

isSymbol :: Char -> Bool
isSymbol c =
  let unacceptableChars = " .0123456789"
      shouldBeIgnored = elem c unacceptableChars
   in not shouldBeIgnored
