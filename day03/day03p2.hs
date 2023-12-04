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

-- Your puzzle answer was 528799.

-- The first half of this puzzle is complete! It provides one gold star: *

-- --- Part Two ---
-- The engineer finds the missing part and installs it in the engine! As the engine springs to life, you jump
-- in the closest gondola, finally ready to ascend to the water source.

-- You don't seem to be going very fast, though. Maybe something is still wrong? Fortunately, the gondola has a phone
-- labeled "help", so you pick it up and the engineer answers.

-- Before you can explain the situation, she suggests that you look out the window. There stands the engineer, holding a
-- phone in one hand and waving with the other. You're going so slowly that you haven't even left the station. You exit the gondola.

-- The missing part wasn't the only issue - one of the gears in the engine is wrong. A gear is any * symbol that is adjacent to
-- exactly two part numbers. Its gear ratio is the result of multiplying those two numbers together.

-- This time, you need to find the gear ratio of every gear and add them all up so that the engineer can figure out
-- which gear needs to be replaced.

-- Consider the same engine schematic again:

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
-- In this schematic, there are two gears. The first is in the top left; it has part numbers 467 and 35, so its gear ratio is 16345.
-- The second gear is in the lower right; its gear ratio is 451490. (The * adjacent to 617 is not a gear because it is only adjacent to one part number.)
-- Adding up all of the gear ratios produces 467835.

-- What is the sum of all of the gear ratios in your engine schematic?

main = interact mainFunc

mainFunc :: String -> String
mainFunc input =
  let (gears, partNumbers) = loadEngineRepresentation $ lines input
      gearsWithNumbers = addNumbersToGears gears partNumbers
      actualGears = catMaybes [if 2 == length partList then Just (gearLocation, partList) else Nothing | (gearLocation, partList) <- gearsWithNumbers]
      ratios = [product partList | (_, partList) <- actualGears]
      sumOfRatios = sum ratios
   in "gears:"
        ++ show gears
        ++ "\n"
        ++ "partNumbers:"
        ++ show partNumbers
        ++ "\n"
        ++ "gearsWithNumbers:"
        ++ show gearsWithNumbers
        ++ "\n"
        ++ "actualGears:"
        ++ show actualGears
        ++ "\n"
        ++ "ratios:"
        ++ show ratios
        ++ "\n"
        ++ "Sum:"
        ++ show sumOfRatios
        ++ "\n"

data Location = Location
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Ord)

addNumbersToGears :: [(Location, Char)] -> [(Location, String)] -> [(Location, [Int])]
addNumbersToGears gears parts =
  let partsFullLocations = [generateLocationsForParts x | x <- parts]
      ints = [read x :: Int | (_, x) <- parts]
      intsWithLocations = zip ints partsFullLocations
      gearsWithParts = [addMatchingPartsToGear g intsWithLocations | g <- gears]
   in gearsWithParts

addMatchingPartsToGear :: (Location, Char) -> [(Int, [Location])] -> (Location, [Int])
addMatchingPartsToGear (gearLocation, _) partsList =
  let gearSurroundingLocations = generateSurroundingLocations gearLocation 1
      overlapIds = catMaybes [if overlaps gearSurroundingLocations numLocations then Just numValue else Nothing | (numValue, numLocations) <- partsList]
      result = (gearLocation, overlapIds)
   in result

overlaps :: [Location] -> [Location] -> Bool
overlaps [] _ = False
overlaps _ [] = False
overlaps a b =
  let intersection = filter (`elem` b) a
   in length intersection > 0

generateLocationsForParts :: (Location, String) -> [Location]
generateLocationsForParts (l, s) = [addX l i | i <- [0 .. length s - 1]]

addX :: Location -> Int -> Location
addX l i = Location (x l + i) (y l)

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

loadEngineRepresentation :: [String] -> ([(Location, Char)], [(Location, String)])
loadEngineRepresentation lines =
  let startingMap = Data.Map.empty
      lineSymbols = [readGearLocations theLine y | (theLine, y) <- zip lines [0 ..]]
      allSymbols = concat lineSymbols
      finalSymbolMap = allSymbols
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

readGearLocations :: String -> Int -> [(Location, Char)]
readGearLocations s yValue =
  let charsAtLocations = zip s [0 ..]
      allLocations = [if isGearSymbol x then Just (pos, yValue, x) else Nothing | (x, pos) <- charsAtLocations]
      remainingOnes = catMaybes allLocations
      result = [(Location a b, c) | (a, b, c) <- remainingOnes]
   in result

isGearSymbol :: Char -> Bool
isGearSymbol c = c == '*'
