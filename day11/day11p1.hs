-- --- Day 11: Cosmic Expansion ---
-- You continue following signs for "Hot Springs" and eventually come across an observatory. The Elf within turns out to be a researcher
-- studying cosmic expansion using the giant telescope here.

-- He doesn't know anything about the missing machine parts; he's only visiting for this research project. However, he confirms that the hot springs are
-- the next-closest area likely to have people; he'll even take you straight there once he's done with today's observation analysis.

-- Maybe you can help him with the analysis to speed things up?

-- The researcher has collected a bunch of data and compiled the data into a single giant image (your puzzle input).
-- The image includes empty space (.) and galaxies (#). For example:

-- ...#......
-- .......#..
-- #.........
-- ..........
-- ......#...
-- .#........
-- .........#
-- ..........
-- .......#..
-- #...#.....
-- The researcher is trying to figure out the sum of the lengths of the shortest path between every pair of galaxies. However, there's a catch:
-- the universe expanded in the time it took the light from those galaxies to reach the observatory.

-- Due to something involving gravitational effects, only some space expands. In fact, the result is that any rows or columns that contain no
-- galaxies should all actually be twice as big.

-- In the above example, three columns and two rows contain no galaxies:

--    v  v  v
--  ...#......
--  .......#..
--  #.........
-- >..........<
--  ......#...
--  .#........
--  .........#
-- >..........<
--  .......#..
--  #...#.....
--    ^  ^  ^
-- These rows and columns need to be twice as big; the result of cosmic expansion therefore looks like this:

-- ....#........
-- .........#...
-- #............
-- .............
-- .............
-- ........#....
-- .#...........
-- ............#
-- .............
-- .............
-- .........#...
-- #....#.......

-- Equipped with this expanded universe, the shortest path between every pair of galaxies can be found. It can help to assign every galaxy a unique number:

-- ....1........
-- .........2...
-- 3............
-- .............
-- .............
-- ........4....
-- .5...........
-- ............6
-- .............
-- .............
-- .........7...
-- 8....9.......
-- In these 9 galaxies, there are 36 pairs. Only count each pair once; order within the pair doesn't matter. For each pair, find any
-- shortest path between the two galaxies using only steps that move up, down, left, or right exactly one . or # at a time.
-- (The shortest path between two galaxies is allowed to pass through another galaxy.)

-- For example, here is one of the shortest paths between galaxies 5 and 9:

-- ....1........
-- .........2...
-- 3............
-- .............
-- .............
-- ........4....
-- .5...........
-- .##.........6
-- ..##.........
-- ...##........
-- ....##...7...
-- 8....9.......
-- This path has length 9 because it takes a minimum of nine steps to get from galaxy 5 to galaxy 9 (the eight locations marked # plus
-- the step onto galaxy 9 itself). Here are some other example shortest path lengths:

-- Between galaxy 1 and galaxy 7: 15
-- Between galaxy 3 and galaxy 6: 17
-- Between galaxy 8 and galaxy 9: 5
-- In this example, after expanding the universe, the sum of the shortest path between all 36 pairs of galaxies is 374.

-- Expand the universe, then find the length of the shortest path between every pair of galaxies. What is the sum of these lengths?

-- To begin, get your puzzle input.

import Data.List (sort)
import Data.Maybe (catMaybes)

main = interact mainFunc

mainFunc :: String -> String
mainFunc input =
  let universe = loadUniverse input
      expandedUniverse = expandUniverse universe
      distance = shortestGalaxyTravel expandedUniverse
   in "\nInitial Universe:\n"
        ++ printUniverse universe
        ++ show universe
        ++ "\nExpanded Universe:\n"
        ++ printUniverse expandedUniverse
        ++ "\nGalaxy Travel Distance: "
        ++ show distance
        ++ "\n"

shortestGalaxyTravel :: Universe -> Int
shortestGalaxyTravel universe =
  let galaxyPairs = allPairs $ galaxies universe
      distances = [calcDistance pair | pair <- galaxyPairs]
   in sum distances

calcDistance :: (Galaxy, Galaxy) -> Int
calcDistance (a, b) =
  let xTravel = abs (theX a - theX b)
      yTravel = abs (theY a - theY b)
   in xTravel + yTravel

expandUniverse :: Universe -> Universe
expandUniverse universe =
  let (minX, maxX, minY, maxY) = getUniverseSize universe
      xs = [theX g | g <- galaxies universe]
      ys = [theY g | g <- galaxies universe]
      xCounts = [(x, length (filter (== x) xs)) | x <- [minX .. maxX]]
      yCounts = [(y, length (filter (== y) ys)) | y <- [minY .. maxY]]
      expandYs = revsort [fst y | y <- filter noGalaxies yCounts]
      expandXs = revsort [fst x | x <- filter noGalaxies xCounts]
      universeA = foldl expandUniverseY universe expandYs
      universeB = foldl expandUniverseX universeA expandXs
   in universeB
  where
    noGalaxies (idx, galaxyCount) = galaxyCount == 0
    revsort = reverse . sort

expandUniverseX :: Universe -> Int -> Universe
expandUniverseX universe targetX =
  let newGalaxyLocations = [if theX g < targetX then g else Galaxy (1 + theX g) (theY g) | g <- galaxies universe]
   in Universe newGalaxyLocations

expandUniverseY :: Universe -> Int -> Universe
expandUniverseY universe targetY =
  let newGalaxyLocations = [if theY g < targetY then g else Galaxy (theX g) (1 + theY g) | g <- galaxies universe]
   in Universe newGalaxyLocations

printUniverse :: Universe -> String
printUniverse universe =
  let (minX, maxX, minY, maxY) = getUniverseSize universe
      sizeString = "Universe size: (" ++ show (maxX - minX) ++ "," ++ show (maxY - minY) ++ ")"
      result = concat [[if Galaxy x y `elem` galaxies universe then '#' else '.' | x <- [minX .. maxX]] ++ "\n" | y <- [minY .. maxY]]
   in sizeString ++ "\n" ++ result

getUniverseSize :: Universe -> (Int, Int, Int, Int)
getUniverseSize universe =
  let xs = [theX g | g <- galaxies universe]
      ys = [theY g | g <- galaxies universe]
      minX = smallest xs
      maxX = largest xs
      minY = smallest ys
      maxY = largest ys
   in (minX, maxX, minY, maxY)

loadUniverse :: String -> Universe
loadUniverse input =
  let theLines = lines input
      linesWithY = zip theLines [0 ..]
      galaxyList = catMaybes $ concat [readGalaxies s 0 y | (s, y) <- linesWithY]
   in Universe galaxyList

readGalaxies :: String -> Int -> Int -> [Maybe Galaxy]
readGalaxies "" _ _ = []
readGalaxies (s : xs) x y = (if s == '#' then Just (Galaxy x y) else Nothing) : readGalaxies xs (x + 1) y

newtype Universe = Universe
  { galaxies :: [Galaxy]
  }
  deriving (Show, Eq)

data Galaxy = Galaxy
  { theX :: Int,
    theY :: Int
  }
  deriving (Show, Eq)

-- Utility functions
splitOn' :: (Eq a) => a -> [a] -> [[a]]
splitOn' _ [] = []
splitOn' delim x =
  let (p1, p2) = break (== delim) x
      remaining = drop 1 p2
   in p1 : splitOn' delim remaining

allSame :: (Eq a) => [a] -> Bool
allSame [x] = True
allSame (x : xs) = if x == head xs then allSame xs else False

largest :: (Ord a) => [a] -> a
largest [x] = x
largest (x : xs) = max x (largest xs)

smallest :: (Ord a) => [a] -> a
smallest [x] = x
smallest (x : xs) = min x $ smallest xs

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (x : xs) = [(x, a) | a <- xs] ++ allPairs xs
