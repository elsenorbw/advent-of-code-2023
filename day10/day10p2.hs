-- --- Day 10: Pipe Maze ---
-- You use the hang glider to ride the hot air from Desert Island all the way up to the floating metal island.
-- This island is surprisingly cold and there definitely aren't any thermals to glide on, so you leave your hang glider behind.

-- You wander around for a while, but you don't find any people or animals. However, you do occasionally find signposts labeled "Hot Springs" pointing in a
-- seemingly consistent direction; maybe you can find someone at the hot springs and ask them where the desert-machine parts are made.

-- The landscape here is alien; even the flowers and trees are made of metal. As you stop to admire some metal grass, you notice something metallic scurry away
-- in your peripheral vision and jump into a big pipe! It didn't look like any animal you've ever seen; if you want a better look, you'll need to get ahead of it.

-- Scanning the area, you discover that the entire field you're standing on is densely packed with pipes; it was hard to tell at first because they're the same
-- metallic silver color as the "ground". You make a quick sketch of all of the surface pipes you can see (your puzzle input).

-- The pipes are arranged in a two-dimensional grid of tiles:

-- \| is a vertical pipe connecting north and south.
-- - is a horizontal pipe connecting east and west.
-- L is a 90-degree bend connecting north and east.
-- J is a 90-degree bend connecting north and west.
-- 7 is a 90-degree bend connecting south and west.
-- F is a 90-degree bend connecting south and east.
-- . is ground; there is no pipe in this tile.
-- S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
-- Based on the acoustics of the animal's scurrying, you're confident the pipe that contains the animal is one large, continuous loop.

-- For example, here is a square loop of pipe:

-- .....
-- .F-7.
-- .|.|.
-- .L-J.
-- .....
-- If the animal had entered this loop in the northwest corner, the sketch would instead look like this:

-- .....
-- .S-7.
-- .|.|.
-- .L-J.
-- .....
-- In the above diagram, the S tile is still a 90-degree F bend: you can tell because of how the adjacent pipes connect to it.

-- Unfortunately, there are also many pipes that aren't connected to the loop! This sketch shows the same loop as above:

-- -L|F7
-- 7S-7|
-- L|7||
-- -L-J|
-- L|-JF
-- In the above diagram, you can still figure out which pipes form the main loop: they're the ones connected to S, pipes those pipes
-- connect to, pipes those pipes connect to, and so on. Every pipe in the main loop connects to its two neighbors
-- (including S, which will have exactly two pipes connecting to it, and which is assumed to connect back to those two pipes).

-- Here is a sketch that contains a slightly more complex main loop:

-- ..F7.
-- .FJ|.
-- SJ.L7

-- \| F--J
--  LJ...
--  Here's the same example sketch with the extra, non-main-loop pipe tiles also shown:

-- 7-F7-
-- .FJ|7
-- SJLL7

-- \| F--J
--  LJ.LJ
--  If you want to get out ahead of the animal, you should find the tile in the loop that is farthest from the starting position.
--  Because the animal is in the pipe,
--  it doesn't make sense to measure this by direct distance.
--  Instead, you need to find the tile that would take the longest number of steps along the loop to reach from the starting point -
--  regardless of which way around the loop the animal went.

-- In the first example with the square loop:

-- .....
-- .S-7.
-- .|.|.
-- .L-J.
-- .....
-- You can count the distance each tile in the loop is from the starting point like this:

-- .....
-- .012.
-- .1.3.
-- .234.
-- .....
-- In this example, the farthest point from the start is 4 steps away.

-- Here's the more complex loop again:

-- ..F7.
-- .FJ|.
-- SJ.L7

-- \| F--J
--  LJ...
--  Here are the distances for each tile on that loop:

-- ..45.
-- .236.
-- 01.78
-- 14567
-- 23...
-- Find the single giant loop starting at S. How many steps along the loop does it take to get from the
-- starting position to the point farthest from the starting position?

-- Your puzzle answer was 6697.

-- The first half of this puzzle is complete! It provides one gold star: *

-- --- Part Two ---
-- You quickly reach the farthest point of the loop, but the animal never emerges. Maybe its nest is within the area enclosed by the loop?

-- To determine whether it's even worth taking the time to search for such a nest, you should calculate how many tiles are contained within the loop. For example:

-- ...........
-- .S-------7.
-- .|F-----7|.
-- .||.....||.
-- .||.....||.
-- .|L-7.F-J|.
-- .|..|.|..|.
-- .L--J.L--J.
-- ...........
-- The above loop encloses merely four tiles - the two pairs of . in the southwest and southeast (marked I below).
-- The middle . tiles (marked O below) are not in the loop. Here is the same loop again with those regions marked:

-- ...........
-- .S-------7.
-- .|F-----7|.
-- .||OOOOO||.
-- .||OOOOO||.
-- .|L-7OF-J|.
-- .|II|O|II|.
-- .L--JOL--J.
-- .....O.....
-- In fact, there doesn't even need to be a full tile path to the outside for tiles to count as outside the loop -
-- squeezing between pipes is also allowed! Here, I is still within the loop and O is still outside the loop:

-- ..........
-- .S------7.
-- .|F----7|.
-- .||OOOO||.
-- .||OOOO||.
-- .|L-7F-J|.
-- .|II||II|.
-- .L--JL--J.
-- ..........
-- In both of the above examples, 4 tiles are enclosed by the loop.

-- Here's a larger example:

-- .F----7F7F7F7F-7....
-- .|F--7||||||||FJ....
-- .||.FJ||||||||L7....
-- FJL7L7LJLJ||LJ.L-7..
-- L--J.L7...LJS7F-7L7.
-- ....F-J..F7FJ|L7L7L7
-- ....L7.F7||L7|.L7L7|
-- .....|FJLJ|FJ|F7|.LJ
-- ....FJL-7.||.||||...
-- ....L---J.LJ.LJLJ...
-- The above sketch has many random bits of ground, some of which are in the loop (I) and some of which are outside it (O):

-- OF----7F7F7F7F-7OOOO
-- O|F--7||||||||FJOOOO
-- O||OFJ||||||||L7OOOO
-- FJL7L7LJLJ||LJIL-7OO
-- L--JOL7IIILJS7F-7L7O
-- OOOOF-JIIF7FJ|L7L7L7
-- OOOOL7IF7||L7|IL7L7|
-- OOOOO|FJLJ|FJ|F7|OLJ
-- OOOOFJL-7O||O||||OOO
-- OOOOL---JOLJOLJLJOOO
-- In this larger example, 8 tiles are enclosed by the loop.

-- Any tile that isn't part of the main loop can count as being enclosed by the loop.
-- Here's another example with many bits of junk pipe lying around that aren't connected to the main loop at all:

-- FF7FSF7F7F7F7F7F---7
-- L|LJ||||||||||||F--J
-- FL-7LJLJ||||||LJL-77
-- F--JF--7||LJLJ7F7FJ-
-- L---JF-JLJ.||-FJLJJ7
-- \|F|F-JF---7F7-L7L|7|
-- \|FFJF7L7F-JF7|JL---7
-- 7-L-JL7||F7|L7F-7F7|
-- L.L7LFJ|||||FJL7||LJ
-- L7JLJL-JLJLJL--JLJ.L
-- Here are just the tiles that are enclosed by the loop marked with I:

-- FF7FSF7F7F7F7F7F---7
-- L|LJ||||||||||||F--J
-- FL-7LJLJ||||||LJL-77
-- F--JF--7||LJLJIF7FJ-
-- L---JF-JLJIIIIFJLJJ7
-- \|F|F-JF---7IIIL7L|7|
-- \|FFJF7L7F-JF7IIL---7
-- 7-L-JL7||F7|L7F-7F7|
-- L.L7LFJ|||||FJL7||LJ
-- L7JLJL-JLJLJL--JLJ.L
-- In this last example, 10 tiles are enclosed by the loop.

-- Figure out whether you have time to search for the nest by calculating the area within the loop.
-- How many tiles are enclosed by the loop?

import Data.Map (Map, empty, findWithDefault, insert, toList)
import Data.Maybe (catMaybes)

main = interact mainFunc

mainFunc :: String -> String
mainFunc input =
  let pipelineMap = loadPipelineMap input
      locationMap = loadLocationMap input
      startPoints = getStartPoints pipelineMap
      initialLocationMap = setWalkLocationsStateInLocationMap Walked locationMap startPoints
      walkResult = walkUntilWeMeetAgain startPoints pipelineMap initialLocationMap
      onlyWalls = removeNonWalls walkResult
      addedOutside = addBorder onlyWalls
   in "pipelineMap: "
        ++ show pipelineMap
        ++ "\nLocationMap:"
        ++ printLocationMap locationMap
        ++ "\nInitialLocationMap:"
        ++ printLocationMap initialLocationMap
        ++ "\nStart points: "
        ++ show startPoints
        ++ "\nwalkResult : "
        ++ printLocationMap walkResult
        ++ "\nonlyWalls : "
        ++ printLocationMap onlyWalls
        ++ "\naddedOutside : "
        ++ printLocationMap addedOutside
        ++ "\n"

data PipelineMap = PipelineMap
  { startingPoint :: Location,
    mapLocations :: Map Location [Location]
  }
  deriving (Show)

data WalkLocation = WalkLocation
  { prev :: Location,
    curr :: Location
  }
  deriving (Show)

data Location = Location
  { theX :: Int,
    theY :: Int
  }
  deriving (Show, Eq, Ord)

data Pipe = Empty | Horizontal | Vertical | TopLeft | TopRight | BottomLeft | BottomRight | Start deriving (Show, Eq)

data PipeState = Unknown | Walked | Inside | Outside deriving (Show, Eq)

newtype LocationMap = LocationMap
  { pipes :: Map Location (Pipe, PipeState)
  }
  deriving (Show)

addBorder :: LocationMap -> LocationMap
addBorder map =
  let (minX, maxX, minY, maxY) = getMapDimensions map
      x1 = minX - 1
      x2 = maxX + 1
      y1 = minY - 1
      y2 = maxY + 1
      north = [Location x y1 | x <- [x1 .. x2]]
      south = [Location x y2 | x <- [x1 .. x2]]
      west = [Location x1 y | y <- [y1 .. y2]]
      east = [Location x2 y | y <- [y1 .. y2]]
      outsideLocations = north ++ south ++ east ++ west
   in foldl (setLocationStateInLocationMap Outside) map outsideLocations

removeNonWalls :: LocationMap -> LocationMap
removeNonWalls map =
  let (minX, maxX, minY, maxY) = getMapDimensions map
      allLocations = [Location x y | x <- [minX .. maxX], y <- [minY .. maxY]]
   in foldl removeUnwalked map allLocations

removeUnwalked :: LocationMap -> Location -> LocationMap
removeUnwalked map location =
  let (pipe, state) = getLocationFromMap location map
      storeVal = if state == Walked then (pipe, state) else (Empty, state)
   in LocationMap (insert location storeVal (pipes map))

printLocationMap :: LocationMap -> String
printLocationMap map =
  let (minX, maxX, minY, maxY) = getMapDimensions map
      rows = [printOneRow map y minX maxX ++ "\n" | y <- [minY .. maxY]]
   in "\n" ++ concat rows

printOneRow :: LocationMap -> Int -> Int -> Int -> String
printOneRow map y x maxx
  | x > maxx = ""
  | otherwise =
      let (pipe, state) = getLocationFromMap (Location x y) map
          c = charForPipe pipe state
       in c : printOneRow map y (x + 1) maxx

getLocationFromMap :: Location -> LocationMap -> (Pipe, PipeState)
getLocationFromMap location map = findWithDefault (Empty, Unknown) location (pipes map)

getMapDimensions :: LocationMap -> (Int, Int, Int, Int)
getMapDimensions map =
  let gridMap = pipes map
      l = toList gridMap
      locations = [fst x | x <- l]
      xs = [theX l | l <- locations]
      ys = [theY l | l <- locations]
   in (smallest xs, largest xs, smallest ys, largest ys)

walkUntilWeMeetAgain :: [WalkLocation] -> PipelineMap -> LocationMap -> LocationMap
walkUntilWeMeetAgain locations map locMap
  | allSame currentLocations = locMap
  | otherwise =
      let nextLocations = [nextStepFor x map | x <- locations]
          locationsToMark = [curr x | x <- nextLocations]
          nextLocationMap = foldl (setLocationStateInLocationMap Walked) locMap locationsToMark
       in walkUntilWeMeetAgain nextLocations map nextLocationMap
  where
    currentLocations = [curr x | x <- locations]

setWalkLocationsStateInLocationMap :: PipeState -> LocationMap -> [WalkLocation] -> LocationMap
setWalkLocationsStateInLocationMap state map walkLocations =
  let actualLocations = [curr x | x <- walkLocations] ++ [prev x | x <- walkLocations]
   in foldl (setLocationStateInLocationMap state) map actualLocations

setLocationStateInLocationMap :: PipeState -> LocationMap -> Location -> LocationMap
setLocationStateInLocationMap state map location =
  let (pipe, _) = getLocationFromMap location map
      newVal = (pipe, state)
   in LocationMap (insert location newVal (pipes map))

nextStepFor :: WalkLocation -> PipelineMap -> WalkLocation
nextStepFor location map =
  let thisLocation = curr location
      prevLocation = prev location
      possibles = getConnections map thisLocation
      actuals = filter (/= prevLocation) possibles
   in if length actuals == 1 then WalkLocation thisLocation (head actuals) else error ("Too many options here.. " ++ show location ++ "->" ++ show actuals)

getStartPoints :: PipelineMap -> [WalkLocation]
getStartPoints theMap =
  let startPos = startingPoint theMap
      possibles = getConnections theMap startPos
      actuals = catMaybes [if startPos `elem` getConnections theMap x then Just x else Nothing | x <- possibles]
      withLastStep = [WalkLocation startPos x | x <- actuals]
   in withLastStep

getConnections :: PipelineMap -> Location -> [Location]
getConnections map location = findWithDefault [] location theMap
  where
    theMap = mapLocations map

loadLocationMap :: String -> LocationMap
loadLocationMap input =
  let dataLines = lines input
      numberedDataLines = zip [0 ..] dataLines
      emptyLocationMap = LocationMap empty
   in foldl addLineToLocationMap emptyLocationMap numberedDataLines

addLineToLocationMap :: LocationMap -> (Int, String) -> LocationMap
addLineToLocationMap a b = addCharToLocationMap a b 0

addCharToLocationMap :: LocationMap -> (Int, String) -> Int -> LocationMap
addCharToLocationMap currentMap (_, "") _ = currentMap
addCharToLocationMap currentMap (y, thisPipe : remainingPipes) x =
  let location = Location x y
      pipe = getPipeFromChar thisPipe
      nextMap = LocationMap (insert location (pipe, Unknown) (pipes currentMap))
   in addCharToLocationMap nextMap (y, remainingPipes) (x + 1)

loadPipelineMap :: String -> PipelineMap
loadPipelineMap input =
  let dataLines = lines input
      numberedDataLines = zip [0 ..] dataLines
      emptyPipelineMap = PipelineMap (Location (-1) (-1)) empty
   in foldl addLineToPipelineMap emptyPipelineMap numberedDataLines

addLineToPipelineMap :: PipelineMap -> (Int, String) -> PipelineMap
addLineToPipelineMap a b = addCharToPipelineMap a b 0

addCharToPipelineMap :: PipelineMap -> (Int, String) -> Int -> PipelineMap
addCharToPipelineMap currentMap (_, "") _ = currentMap
addCharToPipelineMap currentMap (y, thisPipe : remainingPipes) x =
  let location = Location x y
      modStart = if thisPipe == 'S' then PipelineMap location (mapLocations currentMap) else currentMap
      thisPipeValue = lookupPipeValue thisPipe location
      nextMap = PipelineMap (startingPoint modStart) (insert location thisPipeValue (mapLocations modStart))
   in addCharToPipelineMap nextMap (y, remainingPipes) (x + 1)

-- | is a vertical pipe connecting north and south.
-- - is a horizontal pipe connecting east and west.
-- L is a 90-degree bend connecting north and east.
-- J is a 90-degree bend connecting north and west.
-- 7 is a 90-degree bend connecting south and west.
-- F is a 90-degree bend connecting south and east.
-- . is ground; there is no pipe in this tile.
-- S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
-- Based on the acoustics of the animal's scurrying, you're confident the pipe that contains the animal is one large, continuous loop.
lookupPipeValue :: Char -> Location -> [Location]
lookupPipeValue c location
  | c == '|' = [up location, down location]
  | c == '-' = [left location, right location]
  | c == 'L' = [up location, right location]
  | c == 'J' = [up location, left location]
  | c == '7' = [left location, down location]
  | c == 'F' = [right location, down location]
  | c == '.' = []
  | c == 'S' = [up location, left location, right location, down location]
  | otherwise = error ("No idea what you want me to do with " ++ [c])

getPipeFromChar :: Char -> Pipe
getPipeFromChar c
  | c == '|' = Vertical
  | c == '-' = Horizontal
  | c == 'L' = BottomLeft
  | c == 'J' = BottomRight
  | c == '7' = TopRight
  | c == 'F' = TopLeft
  | c == '.' = Empty
  | c == 'S' = Start
  | otherwise = error ("No idea what you want me to do with " ++ [c])

charForPipe :: Pipe -> PipeState -> Char
charForPipe pipe state
  | state == Inside = 'I'
  | state == Outside = 'O'
  | (state == Walked || state == Unknown) && pipe == Vertical = '|'
  | (state == Walked || state == Unknown) && pipe == Horizontal = '-'
  | (state == Walked || state == Unknown) && pipe == BottomLeft = 'L'
  | (state == Walked || state == Unknown) && pipe == BottomRight = 'J'
  | (state == Walked || state == Unknown) && pipe == TopRight = '7'
  | (state == Walked || state == Unknown) && pipe == TopLeft = 'F'
  | (state == Walked || state == Unknown) && pipe == Empty = ' '
  | (state == Walked || state == Unknown) && pipe == Start = 'S'
  | otherwise = error ("I can't give you a char for state=" ++ show state ++ "," ++ show state)

up :: Location -> Location
up (Location x y) = Location x (y - 1)

down :: Location -> Location
down (Location x y) = Location x (y + 1)

right :: Location -> Location
right (Location x y) = Location (x + 1) y

left :: Location -> Location
left (Location x y) = Location (x - 1) y

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
largest (x : xs) = foldl max x xs

smallest :: (Ord a) => [a] -> a
smallest (x : xs) = foldl min x xs