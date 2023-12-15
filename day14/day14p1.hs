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

data Platform = Platform {
  maxx :: Int,
  maxy :: Int,
  fixedRocks :: [(Int, Int)],
  rollers :: [(Int, Int)] 
} deriving (Show)


main = interact mainFunc

mainFunc :: String -> String
mainFunc input = 
   let platform = loadPlatform input
       rolledPlatform = rollPlatformNorth platform
       weight = calculateLoad rolledPlatform
   in showPlatform platform 
      ++ "\nRolled:\n"
      ++ showPlatform rolledPlatform
      ++ "\nTotal Load: "
      ++ show weight 
      ++ "\n"

calculateLoad :: Platform -> Int
calculateLoad plat =
   let rocks = rollers plat 
       platformLength = 1 + maxx plat
       individualLoads = [platformLength - y | (_, y) <- rocks]
       totalLoad = sum individualLoads
   in totalLoad 

rollPlatformNorth :: Platform -> Platform
rollPlatformNorth plat = 
   let a = rollers plat 
       b = [rollNorth x plat | x <- a]
       newPlat = Platform (maxx plat) (maxy plat) (fixedRocks plat) b
   in if a == b then plat else rollPlatformNorth newPlat 

rollNorth :: (Int, Int) -> Platform -> (Int, Int)
rollNorth (x, y) plat = if canRollInto x (y-1) plat then (x, y-1) else (x, y)

showPlatform :: Platform -> String
showPlatform plat =
   let outlines = [[getPlatformChar x y plat | x <- [0..maxx plat]] ++ "\n" | y <- [0.. maxy plat]]
   in  
   "Platform " 
   ++ show (maxx plat + 1) ++ "," ++ show (maxy plat + 1) ++ "\n"
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
       fixedB =  concat [readBoulders 0 y '#' s | (y, s) <- zip [0..] choppedLines]
       rollingB = concat [readBoulders 0 y 'O' s | (y, s) <- zip [0..] choppedLines]
   in Platform x y fixedB rollingB 

readBoulders :: Int -> Int -> Char -> String -> [(Int, Int)]
readBoulders _ _ _ "" = []
readBoulders x y boulderChar (s:xs) = 
   if s == boulderChar then (x, y): nextBoulders else nextBoulders
   where nextBoulders = readBoulders (x+1) y boulderChar xs
   

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
