-- --- Day 14: Parabolic Reflector Dish ---
-- You reach the place where all of the mirrors were pointing: a massive parabolic reflector dish attached to the side of another large mountain.

-- The dish is made up of many small mirrors, but while the mirrors themselves are roughly in the shape of a parabolic reflector dish, each individual
-- mirror seems to be pointing in slightly the wrong direction. If the dish is meant to focus light, all it's doing right now is sending it in a vague direction.

-- This system must be what provides the energy for the lava! If you focus the reflector dish, maybe you can go where it's pointing and use the light to fix the lava production.

-- Upon closer inspection, the individual mirrors each appear to be connected via an elaborate system of ropes and pulleys to a large metal platform below the dish.
-- The platform is covered in large rocks of various shapes. Depending on their position, the weight of the rocks deforms the platform, and the shape of the platform
-- controls which ropes move and ultimately the focus of the dish.

-- In short: if you move the rocks, you can focus the dish. The platform even has a control panel on the side that lets you tilt it in one of four directions!
--    The rounded rocks (O) will roll when the platform is tilted, while the cube-shaped rocks (#) will stay in place.
--    You note the positions of all of the empty spaces (.) and rocks (your puzzle input). For example:

-- O....#....
-- O.OO#....#
-- .....##...
-- OO.#O....O
-- .O.....O#.
-- O.#..O.#.#
-- ..O..#O..O
-- .......O..
-- #....###..
-- #OO..#....
-- Start by tilting the lever so all of the rocks will slide north as far as they will go:

-- OOOO.#.O..
-- OO..#....#
-- OO..O##..O
-- O..#.OO...
-- ........#.
-- ..#....#.#
-- ..O..#.O.O
-- ..O.......
-- #....###..
-- #....#....
-- You notice that the support beams along the north side of the platform are damaged; to ensure the platform doesn't collapse,
-- you should calculate the total load on the north support beams.

-- The amount of load caused by a single rounded rock (O) is equal to the number of rows from the rock to the south edge of the platform,
-- including the row the rock is on. (Cube-shaped rocks (#) don't contribute to load.)
-- So, the amount of load caused by each rock in each row is as follows:

-- OOOO.#.O.. 10
-- OO..#....#  9
-- OO..O##..O  8
-- O..#.OO...  7
-- ........#.  6
-- ..#....#.#  5
-- ..O..#.O.O  4
-- ..O.......  3
-- #....###..  2
-- #....#....  1
-- The total load is the sum of the load caused by all of the rounded rocks. In this example, the total load is 136.

-- Tilt the platform so that the rounded rocks all roll north. Afterward, what is the total load on the north support beams?

-- To begin, get your puzzle input.

-- Your puzzle answer was 105249.

-- The first half of this puzzle is complete! It provides one gold star: *

-- --- Part Two ---
-- The parabolic reflector dish deforms, but not in a way that focuses the beam. To do that, you'll need to move the rocks to the edges of
--    the platform. Fortunately, a button on the side of the control panel labeled "spin cycle" attempts to do just that!

-- Each cycle tilts the platform four times so that the rounded rocks roll north, then west, then south, then east. After each tilt, the rounded
-- rocks roll as far as they can before the platform tilts in the next direction. After one cycle, the platform will have finished rolling the
-- rounded rocks in those four directions in that order.

-- Here's what happens in the example above after each of the first few cycles:

-- After 1 cycle:
-- .....#....
-- ....#...O#
-- ...OO##...
-- .OO#......
-- .....OOO#.
-- .O#...O#.#
-- ....O#....
-- ......OOOO
-- #...O###..
-- #..OO#....

-- After 2 cycles:
-- .....#....
-- ....#...O#
-- .....##...
-- ..O#......
-- .....OOO#.
-- .O#...O#.#
-- ....O#...O
-- .......OOO
-- #..OO###..
-- #.OOO#...O

-- After 3 cycles:
-- .....#....
-- ....#...O#
-- .....##...
-- ..O#......
-- .....OOO#.
-- .O#...O#.#
-- ....O#...O
-- .......OOO
-- #...O###.O
-- #.OOO#...O
-- This process should work if you leave it running long enough, but you're still worried about the north support beams.
-- To make sure they'll survive for a while, you need to calculate the total load on the north support beams after 1000000000 cycles.

-- In the above example, after 1000000000 cycles, the total load on the north support beams is 64.

-- Run the spin cycle for 1000000000 cycles. Afterward, what is the total load on the north support beams?

-- So presumably the state either reaches equilibrium or has a looping cycle, let's run a few to find out which and that will determine the solution.
-- There are obviously too many rocks and too many iterations for the brute-force attack

import Data.Hashable
import Data.List (elemIndex, sortBy)
import Data.Maybe (fromJust)

data Platform = Platform
  { maxx :: Int,
    maxy :: Int,
    fixedRocks :: [(Int, Int)],
    rollers :: [(Int, Int)]
  }
  deriving (Show)

main = interact mainFunc

mainFunc :: String -> String
mainFunc input =
  let platform = loadPlatform input
      starter = runOneSpinCycle platform
      (loopsAt, loopsTo, platformInfo) = findSequenceLoop [hashAndScoreFor platform, hashAndScoreFor starter] starter
      weight = getLoadAfterXIterations 1_000_000_000 loopsAt loopsTo platformInfo
   in showPlatform platform
        ++ "\nScores:\n"
        ++ concat [show idx ++ " -> " ++ show x ++ "\n" | (x, idx) <- zip platformInfo [0 ..]]
        ++ "\nloopsAt:\n"
        ++ show loopsAt
        ++ "\nloopsTo:\n"
        ++ show loopsTo
        ++ "\nTotal Load: "
        ++ show weight
        ++ "\n"

getLoadAfterXIterations :: Int -> Int -> Int -> [(Int, Int)] -> Int
getLoadAfterXIterations targetIterations loopsAt loopsTo platformInfo =
  let wrapVal = targetIterations - loopsTo -- we can ignore the initial states, they are not part of the loop
      loopLength = loopsAt - loopsTo -- every time we complete this length we are back at the start again
      eventualOffset = wrapVal `mod` loopLength
      eventualIdx = loopsTo + eventualOffset
   in snd (platformInfo !! eventualIdx)

-- returns the index at which it loops, where it loops back to and the historical list of hashes and scores
findSequenceLoop :: [(Int, Int)] -> Platform -> (Int, Int, [(Int, Int)])
findSequenceLoop history plat =
  let nextPlat = runOneSpinCycle plat
      nextScore = hashAndScoreFor nextPlat
      nextHistory = history ++ [nextScore]
   in if nextScore `elem` history
        then (length history, fromJust (nextScore `elemIndex` history), nextHistory)
        else findSequenceLoop nextHistory nextPlat

hashAndScoreFor :: Platform -> (Int, Int)
hashAndScoreFor plat = (hashRollers plat, calculateLoad plat)

calculateLoad :: Platform -> Int
calculateLoad plat =
  let rocks = rollers plat
      platformLength = 1 + maxx plat
      individualLoads = [platformLength - y | (_, y) <- rocks]
      totalLoad = sum individualLoads
   in totalLoad

hashRollers :: Platform -> Int
hashRollers plat =
  let r = rollers plat
      s = sortBy xTheny r
      h = hash s
   in h

-- Each cycle tilts the platform four times so that the rounded rocks roll north, then west, then south, then east. After each tilt, the rounded
-- rocks roll as far as they can before the platform tilts in the next direction. After one cycle, the platform will have finished rolling the
-- rounded rocks in those four directions in that order.

runXSpinCycles :: Int -> Platform -> [Platform]
runXSpinCycles 0 _ = []
runXSpinCycles x plat =
  let thisOne = runOneSpinCycle plat
      remaining = x - 1
   in thisOne : runXSpinCycles remaining thisOne

runOneSpinCycle :: Platform -> Platform
runOneSpinCycle plat =
  let a = rollPlatformNorth plat
      b = rollPlatformWest a
      c = rollPlatformSouth b
      d = rollPlatformEast c
   in d

rollPlatformNorth :: Platform -> Platform
rollPlatformNorth plat =
  let a = rollers plat
      b = sortNorth a
      c = [rollNorth x plat | x <- b]
      newPlat = Platform (maxx plat) (maxy plat) (fixedRocks plat) c
   in if b == c then plat else rollPlatformNorth newPlat

rollPlatformSouth :: Platform -> Platform
rollPlatformSouth plat =
  let a = rollers plat
      b = sortSouth a
      c = [rollSouth x plat | x <- b]
      newPlat = Platform (maxx plat) (maxy plat) (fixedRocks plat) c
   in if b == c then plat else rollPlatformSouth newPlat

rollPlatformEast :: Platform -> Platform
rollPlatformEast plat =
  let a = rollers plat
      b = sortEast a
      c = [rollEast x plat | x <- b]
      newPlat = Platform (maxx plat) (maxy plat) (fixedRocks plat) c
   in if b == c then plat else rollPlatformEast newPlat

rollPlatformWest :: Platform -> Platform
rollPlatformWest plat =
  let a = rollers plat
      b = sortWest a
      c = [rollWest x plat | x <- b]
      newPlat = Platform (maxx plat) (maxy plat) (fixedRocks plat) c
   in if b == c then plat else rollPlatformWest newPlat

smallY :: (Int, Int) -> (Int, Int) -> Ordering
smallY (x1, y1) (x2, y2) = compare y1 y2

smallX :: (Int, Int) -> (Int, Int) -> Ordering
smallX (x1, y1) (x2, y2) = compare x1 x2

xTheny :: (Int, Int) -> (Int, Int) -> Ordering
xTheny (x1, y1) (x2, y2) = if x1 == x2 then smallY (x1, y1) (x2, y2) else smallX (x1, y1) (x2, y2)

-- sort the list items so that the ones already Northmost go first
-- makes the remaining logic way simpler
sortNorth :: [(Int, Int)] -> [(Int, Int)]
sortNorth = sortBy smallY

sortSouth :: [(Int, Int)] -> [(Int, Int)]
sortSouth = reverse . sortNorth

sortWest :: [(Int, Int)] -> [(Int, Int)]
sortWest = sortBy smallX

sortEast :: [(Int, Int)] -> [(Int, Int)]
sortEast = reverse . sortWest

rollDirection :: (Int, Int) -> (Int, Int) -> Platform -> (Int, Int)
rollDirection (xinc, yinc) (x, y) plat = if canRollInto (x + xinc) (y + yinc) plat then (x + xinc, y + yinc) else (x, y)

rollNorth :: (Int, Int) -> Platform -> (Int, Int)
rollNorth = rollDirection (0, -1)

rollSouth :: (Int, Int) -> Platform -> (Int, Int)
rollSouth = rollDirection (0, 1)

rollWest :: (Int, Int) -> Platform -> (Int, Int)
rollWest = rollDirection (-1, 0)

rollEast :: (Int, Int) -> Platform -> (Int, Int)
rollEast = rollDirection (1, 0)

showPlatform :: Platform -> String
showPlatform plat =
  let outlines = [[getPlatformChar x y plat | x <- [0 .. maxx plat]] ++ "\n" | y <- [0 .. maxy plat]]
   in "Platform "
        ++ show (maxx plat + 1)
        ++ ","
        ++ show (maxy plat + 1)
        ++ " hash={"
        ++ show (hashRollers plat)
        ++ "}\n"
        ++ concat outlines
        ++ "\n"

getPlatformChar :: Int -> Int -> Platform -> Char
getPlatformChar x y plat
  | isFixed x y plat = '#'
  | isRolling x y plat = 'O'
  | otherwise = '.'

isFixed :: Int -> Int -> Platform -> Bool
isFixed x y plat = (x, y) `elem` fixedRocks plat

isRolling :: Int -> Int -> Platform -> Bool
isRolling x y plat = (x, y) `elem` rollers plat

inBounds :: Int -> Int -> Platform -> Bool
inBounds x y plat =
  let xOK = x >= 0 && x <= maxx plat
      yOK = y >= 0 && y <= maxy plat
   in xOK && yOK

canRollInto :: Int -> Int -> Platform -> Bool
canRollInto x y plat =
  let can1 = not (isFixed x y plat)
      can2 = not (isRolling x y plat)
      can3 = inBounds x y plat
   in can1 && can2 && can3

loadPlatform :: String -> Platform
loadPlatform input =
  let choppedLines = lines input
      x = length choppedLines - 1
      y = length (head choppedLines) - 1
      fixedB = concat [readBoulders 0 y '#' s | (y, s) <- zip [0 ..] choppedLines]
      rollingB = concat [readBoulders 0 y 'O' s | (y, s) <- zip [0 ..] choppedLines]
   in Platform x y fixedB rollingB

readBoulders :: Int -> Int -> Char -> String -> [(Int, Int)]
readBoulders _ _ _ "" = []
readBoulders x y boulderChar (s : xs) =
  if s == boulderChar then (x, y) : nextBoulders else nextBoulders
  where
    nextBoulders = readBoulders (x + 1) y boulderChar xs

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
