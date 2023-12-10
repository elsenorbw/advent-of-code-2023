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

import Data.Map (Map, empty, findWithDefault, insert)
import Data.Maybe (catMaybes)

main = interact mainFunc

mainFunc :: String -> String
mainFunc input =
  let pipelineMap = loadPipelineMap input
      startPoints = getStartPoints pipelineMap
      walkResult = walkUntilWeMeetAgain startPoints 1 pipelineMap
   in "pipelineMap: "
        ++ show pipelineMap
        ++ "\nStart points: "
        ++ show startPoints
        ++ "\nwalkResult : "
        ++ show walkResult
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

walkUntilWeMeetAgain :: [WalkLocation] -> Int -> PipelineMap -> (Location, Int)
walkUntilWeMeetAgain locations stepsSoFar map
  | allSame currentLocations = (head currentLocations, stepsSoFar)
  | otherwise =
      let nextLocations = [nextStepFor x map | x <- locations]
       in walkUntilWeMeetAgain nextLocations (stepsSoFar + 1) map
  where
    currentLocations = [curr x | x <- locations]

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
