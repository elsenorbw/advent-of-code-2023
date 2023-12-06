-- --- Day 5: If You Give A Seed A Fertilizer ---
-- You take the boat and find the gardener right where you were told he would be: managing a giant "garden" that looks more to you like a farm.

-- "A water source? Island Island is the water source!" You point out that Snow Island isn't receiving any water.

-- "Oh, we had to stop the water because we ran out of sand to filter it with! Can't make snow with dirty water. Don't worry, I'm sure we'll
-- get more sand soon; we only turned off the water a few days... weeks... oh no." His face sinks into a look of horrified realization.

-- "I've been so busy making sure everyone here has food that I completely forgot to check why we stopped getting more sand! There's a ferry
-- leaving soon that is headed over in that direction - it's much faster than your boat. Could you please go check it out?"

-- You barely have time to agree to this request when he brings up another. "While you wait for the ferry, maybe you can help us with our food
-- production problem. The latest Island Island Almanac just arrived and we're having trouble making sense of it."

-- The almanac (your puzzle input) lists all of the seeds that need to be planted. It also lists what type of soil to use with each kind of seed,
-- what type of fertilizer to use with each kind of soil, what type of water to use with each kind of fertilizer, and so on.
-- Every type of seed, soil, fertilizer and so on is identified with a number, but numbers are reused by each category - that is, soil 123
-- and fertilizer 123 aren't necessarily related to each other.

-- For example:

-- seeds: 79 14 55 13

-- seed-to-soil map:
-- 50 98 2
-- 52 50 48

-- soil-to-fertilizer map:
-- 0 15 37
-- 37 52 2
-- 39 0 15

-- fertilizer-to-water map:
-- 49 53 8
-- 0 11 42
-- 42 0 7
-- 57 7 4

-- water-to-light map:
-- 88 18 7
-- 18 25 70

-- light-to-temperature map:
-- 45 77 23
-- 81 45 19
-- 68 64 13

-- temperature-to-humidity map:
-- 0 69 1
-- 1 0 69

-- humidity-to-location map:
-- 60 56 37
-- 56 93 4

-- The almanac starts by listing which seeds need to be planted: seeds 79, 14, 55, and 13.

-- The rest of the almanac contains a list of maps which describe how to convert numbers from a source category into numbers
-- in a destination category. That is, the section that starts with seed-to-soil map: describes how to convert a seed number (the source) to
-- a soil number (the destination). This lets the gardener and his team know which soil to use with which seeds, which water to use with which fertilizer, and so on.

-- Rather than list every source number and its corresponding destination number one by one, the maps describe entire ranges of numbers that can be converted.
-- Each line within a map contains three numbers: the destination range start, the source range start, and the range length.

-- Consider again the example seed-to-soil map:

-- 50 98 2
-- 52 50 48
-- The first line has a destination range start of 50, a source range start of 98, and a range length of 2.
-- This line means that the source range starts at 98 and contains two values: 98 and 99. The destination range is the same length, but it starts at 50,
-- so its two values are 50 and 51. With this information, you know that seed number 98 corresponds to soil number 50 and that seed number 99 corresponds to soil number 51.

-- The second line means that the source range starts at 50 and contains 48 values: 50, 51, ..., 96, 97. This corresponds to a destination range starting at 52
-- and also containing 48 values: 52, 53, ..., 98, 99. So, seed number 53 corresponds to soil number 55.

-- Any source numbers that aren't mapped correspond to the same destination number. So, seed number 10 corresponds to soil number 10.

-- So, the entire list of seed numbers and their corresponding soil numbers looks like this:

-- seed  soil
-- 0     0
-- 1     1
-- ...   ...
-- 48    48
-- 49    49
-- 50    52
-- 51    53
-- ...   ...
-- 96    98
-- 97    99
-- 98    50
-- 99    51
-- With this map, you can look up the soil number required for each initial seed number:

-- Seed number 79 corresponds to soil number 81.
-- Seed number 14 corresponds to soil number 14.
-- Seed number 55 corresponds to soil number 57.
-- Seed number 13 corresponds to soil number 13.
-- The gardener and his team want to get started as soon as possible, so they'd like to know the closest location that needs a seed.
-- Using these maps, find the lowest location number that corresponds to any of the initial seeds.
-- To do this, you'll need to convert each seed number through other categories until you can find its corresponding location number.
-- In this example, the corresponding types are:

-- Seed 79, soil 81, fertilizer 81, water 81, light 74, temperature 78, humidity 78, location 82.
-- Seed 14, soil 14, fertilizer 53, water 49, light 42, temperature 42, humidity 43, location 43.
-- Seed 55, soil 57, fertilizer 57, water 53, light 46, temperature 82, humidity 82, location 86.
-- Seed 13, soil 13, fertilizer 52, water 41, light 34, temperature 34, humidity 35, location 35.
-- So, the lowest location number in this example is 35.

-- What is the lowest location number that corresponds to any of the initial seed numbers?

-- Your puzzle answer was 313045984.

-- The first half of this puzzle is complete! It provides one gold star: *

-- --- Part Two ---
-- Everyone will starve if you only plant such a small number of seeds. Re-reading the almanac, it looks like the seeds:
--   line actually describes ranges of seed numbers.

-- The values on the initial seeds: line come in pairs. Within each pair, the first value is the start of the range and
-- the second value is the length of the range. So, in the first line of the example above:

-- seeds: 79 14 55 13
-- This line describes two ranges of seed numbers to be planted in the garden.
-- The first range starts with seed number 79 and contains 14 values: 79, 80, ..., 91, 92.
-- The second range starts with seed number 55 and contains 13 values: 55, 56, ..., 66, 67.

-- Now, rather than considering four seed numbers, you need to consider a total of 27 seed numbers.

-- In the above example, the lowest location number can be obtained from seed number 82, which corresponds to soil 84, fertilizer 84, water 84, light 77, temperature 45, humidity 46, and location 46.
-- So, the lowest location number is 46.

-- Consider all of the initial seed numbers listed in the ranges on the first line of the almanac. What is the lowest location number that corresponds to any of the initial seed numbers?

-- god damn this is annoying. HAd a plan and the machine restarted...
-- something about overlapping ranges
-- ok, initially there is a single range of 0+ which means that we only need to look at the smallest value in that range for each of the seed ranges - simple 100..1000000000 is 100 easy
-- let's imagine a final map step that affects the values of 100-200 and 400-500
-- now we effectively have 5 ranges to worry about : 0-99, 100-200, 201-399, 400-500, 501+
-- so far so good, and if we have our seed range of say 80-10000 then the actual evaluations we need to do are the smallest values we have in each range
-- this should be true as all subsequent values in that range will necessarily finish higher so:
-- 0-99 -> 80
-- 100-200 -> 100
-- 201-399 -> 201
-- 400-500 -> 400
-- 501+ -> 501
-- so a nice efficiency saving there, but we need to handle multiple ranges in sequence so.. what if we add a layer in front that has two ranges of 80-120 and 350-600 ?
-- 0-99 -> 0-79, 80-99
-- 100-200 -> 100-120, 121-200
-- 201-399 -> 201-349, 350-399
-- 400-500 -> 400-500
-- 501+ -> 501-600, 601+

-- leaving us with..
-- 0-79, 80-99, 100-120, 121-200, 201-349, 350-399, 400-500, 501-600, 601+
-- still a good reduction in complexity
-- we need to figure out the rules for splitting the sections to make the code easy though so..
-- using the notation original_l, original_h new_l, new_h -> ol,oh, nl,nh
-- situation 1 : the new section fits entirely within the old section e.g. we add 20-40 into 10-100 - this is easy, ol-nl-1, nl-nh, nh+1-oh / any zero length segments can be removed in the cases where ol==nl or oh==nh
-- Situation 2 : the old section fits entirely within the new section e.g. we add 0-300 into 10-100 - again, easy nl-ol-1, ol-oh, oh+1-nh / so this is identical to the situation above but in reverse
-- Situation 3 : the new section covers the bottom half of the old section e.g. we add 5-20 into 10-100 - .. wait.. if there is any overlap at all, can we just sort the numbers and build ranges from them ?

-- Sit1: 20,40,10,100 -> 10,20,40,100 -> 10-19, 20-40, 41-100
-- Sit2: 0,300,10,100 -> 0,10,100,300 -> 0-9, 10-100, 101-300
-- Sit3  5,20,10,100 -> 5, 10, 20, 100 -> 0-4, 5-.. wait - we are only interested here in splitting up the existing segment where necessary.
-- If the incoming segment starts below the start of this segment then we have no issue as it will have been handled in the previous segment
-- If the incoming segment finishes after the end of this segment then we have no issue as it will be handled in the next segment
-- So.. if either end is inside this segment (not equal??) then we need to split this segment up to be the bit before the start or end of the incoming segment and the bit afterwards.
-- in fact, this is so true that we can treat either end of the incoming segment as a single place where the rules change - as such this is the start of a new segment
-- let's consider that option then..
-- we start with a range of 0 to infinity then we add a first rule - 10-100. So we take the 10 - is it inside the range 0..Inf - yes so now we split that range into 0..9 and 10..Inf
-- so far so hoopy : now 100 - does it interect 0..9 ? No, does it intersect 10..Inf yes so split that at 100 giving us 10..100 and 101..Inf - noticing a small problem here in
-- that the start value is where the new behaviours begin but the new behaviours end one AFTER the end value so we'd need to break on the start point and then break on end-point+1
-- no great shakes.
-- so we can effectively keep adding the rules from all the different translation layers and then for each seed range pick the lowest possible available seed value in that range.
-- there are probably more efficiencies to be had here, but this feels like it'll be enough. There are 220 lines in the puzzle input, meaning that the rule count is around 200,
-- meaning we'd be generating a max of 400 ish segments. That makes it 400 evaluations per seed range, with 10 seed ranges, 4k lookups and we have a computer. Sold.
-- it's a plan, but it's too late to implement now, something for a quiet moment tomorrow.

-- the plan is wrong. We need to remember that after layer 1, the new ranges are adjusted and it's those adjusted numbers that need to be further split down.
-- basics were ok, but we need to be applying the range modifiers to the new set of rules on the next layer.
-- the good news here is that we are effectively flattening all the transformations which gives us a single adjustment layer, making the calcs faster aftwerwards.
-- still need to have a think about the logic for merging the layers though, but tomorrow.

import Data.Maybe (catMaybes)

main = interact mainFunc

mainFunc :: String -> String
mainFunc input =
  let inputVals = lines input
      seedsLine = head inputVals
      seedsInfo = drop 1 $ splitOn' ' ' seedsLine
      tablesSection = drop 2 inputVals
      tableSections = splitOn' "" tablesSection
      conversionTables = [readConversionTable x | x <- tableSections]
      -- and generate all the starting segments
      startingSegments = generateSegments conversionTables [(0, maxBound :: Int)]
      inputSeedRanges = [read x :: Int | x <- seedsInfo]
      inputSeeds = extrapolateRanges inputSeedRanges
      locations = [runConversions x conversionTables | x <- inputSeeds]
      nearest = smallestOne locations
   in "Lines:"
        ++ show inputVals
        ++ "\nTables:"
        ++ show tableSections
        ++ "\n\nconversionTables:"
        ++ show conversionTables
        ++ "\n\nstartingSegments:"
        ++ show startingSegments
        ++ "\n\ninputSeedRanges:"
        ++ show inputSeedRanges
        ++ "\n\ninputSeeds:"
        ++ show inputSeeds
        ++ "\n\nLocations:"
        ++ show locations
        ++ "\n\nNearest: "
        ++ show nearest
        ++ "\n"

data ConversionRule = ConversionRule
  { rangeStart :: Int,
    rangeEnd :: Int,
    modifierValue :: Int
  }
  deriving (Eq, Show)

data ConversionTable = ConversionTable
  { tableName :: String,
    rules :: [ConversionRule]
  }
  deriving (Eq, Show)

generateSegments :: [ConversionTable] -> [(Int, Int)] -> [(Int, Int)]
generateSegments [] x = x
generateSegments tables segments =
  let thisTable = head tables
      remainingTables = drop 1 tables
      newSegments = foldl addSegmentSplit segments $ rules thisTable
   in generateSegments remainingTables newSegments

addSegmentSplit :: [(Int, Int)] -> ConversionRule -> [(Int, Int)]
addSegmentSplit segments rule =
  let modifiedSegments = addOneSplitPoint segments $ rangeStart rule
      finalSegments = addOneSplitPoint modifiedSegments $ 1 + rangeEnd rule
   in finalSegments

addOneSplitPoint :: [(Int, Int)] -> Int -> [(Int, Int)]
addOneSplitPoint [] _ = []
addOneSplitPoint (x : xs) splitPoint =
  if fst x < splitPoint && snd x > splitPoint then (fst x, splitPoint - 1) : (splitPoint, snd x) : xs else x : addOneSplitPoint xs splitPoint

extrapolateRanges :: [Int] -> [(Int, Int)] -> [Int]
extrapolateRanges [] _ = []
extrapolateRanges ranges startSegments =
  let a = head ranges
      b = ranges !! 1
      thisRange = [a .. a + b - 1]
      -- fix this part and we're golden
      remainingRanges = drop 2 ranges
   in thisRange ++ (extrapolateRanges remainingRanges)

runConversions :: Int -> [ConversionTable] -> Int
runConversions x [] = x
runConversions x tableList =
  let thisTable = head tableList
      convertedValues = catMaybes [if x >= rangeStart r && x <= rangeEnd r then Just (x + modifierValue r) else Nothing | r <- rules thisTable]
      newX = if null convertedValues then x else head convertedValues
      remainingTables = drop 1 tableList
   in runConversions newX remainingTables

readConversionTable :: [String] -> ConversionTable
readConversionTable tableLines =
  let nameStr = head $ splitOn' ' ' (head tableLines)
      ruleLines = drop 1 tableLines
      ruleList = [readRule x | x <- ruleLines]
   in ConversionTable nameStr ruleList

readRule :: String -> ConversionRule
readRule s =
  let parts = splitOn' ' ' s
      intParts = [read x :: Int | x <- parts]
      (destStart : sourceStart : rangeLength : _) = intParts
      requiredOffset = destStart - sourceStart
      sourceEnd = sourceStart + rangeLength - 1
   in ConversionRule sourceStart sourceEnd requiredOffset

-- Utility function
splitOn' :: (Eq a) => a -> [a] -> [[a]]
splitOn' _ [] = []
splitOn' delim x =
  let (p1, p2) = break (== delim) x
      remaining = drop 1 p2
   in p1 : splitOn' delim remaining

smallestOne :: (Ord a) => [a] -> a
smallestOne [] = error "smallestOne called with an empty list"
smallestOne [x] = x
smallestOne (x : xs) = min x (smallestOne xs)
