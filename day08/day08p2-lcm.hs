-- --- Day 8: Haunted Wasteland ---
-- You're still riding a camel across Desert Island when you spot a sandstorm quickly approaching. When you turn to warn the Elf,
-- she disappears before your eyes! To be fair, she had just finished warning you about ghosts a few minutes ago.

-- One of the camel's pouches is labeled "maps" - sure enough, it's full of documents (your puzzle input) about how to navigate the desert.
-- At least, you're pretty sure that's what they are; one of the documents contains a list of left/right instructions, and the rest of the
-- documents seem to describe some kind of network of labeled nodes.

-- It seems like you're meant to use the left/right instructions to navigate the network.
-- Perhaps if you have the camel follow the same instructions, you can escape the haunted wasteland!

-- After examining the maps for a bit, two nodes stick out: AAA and ZZZ. You feel like AAA is where you are now,
-- and you have to follow the left/right instructions until you reach ZZZ.

-- This format defines each node of the network individually. For example:

-- RL

-- AAA = (BBB, CCC)
-- BBB = (DDD, EEE)
-- CCC = (ZZZ, GGG)
-- DDD = (DDD, DDD)
-- EEE = (EEE, EEE)
-- GGG = (GGG, GGG)
-- ZZZ = (ZZZ, ZZZ)
-- Starting with AAA, you need to look up the next element based on the next left/right instruction in your input.
-- In this example, start with AAA and go right (R) by choosing the right element of AAA, CCC. Then, L means to choose the left element of CCC, ZZZ.
-- By following the left/right instructions, you reach ZZZ in 2 steps.

-- Of course, you might not find ZZZ right away. If you run out of left/right instructions, repeat the whole sequence of instructions as necessary:
-- RL really means RLRLRLRLRLRLRLRL... and so on. For example, here is a situation that takes 6 steps to reach ZZZ:

-- LLR

-- AAA = (BBB, BBB)
-- BBB = (AAA, ZZZ)
-- ZZZ = (ZZZ, ZZZ)
-- Starting at AAA, follow the left/right instructions. How many steps are required to reach ZZZ?

-- Your puzzle answer was 19951.

-- The first half of this puzzle is complete! It provides one gold star: *

-- --- Part Two ---
-- The sandstorm is upon you and you aren't any closer to escaping the wasteland. You had the camel follow the instructions, but you've barely left your starting position.
-- It's going to take significantly more steps to escape!

-- What if the map isn't for people - what if the map is for ghosts? Are ghosts even bound by the laws of spacetime? Only one way to find out.

-- After examining the maps a bit longer, your attention is drawn to a curious fact:

-- the number of nodes with names ending in A is equal to the number ending in Z!
-- If you were a ghost, you'd probably just start at every node that ends with A and follow all of the paths at
-- the same time until they all simultaneously end up at nodes that end with Z.

-- For example:

-- LR

-- 11A = (11B, XXX)
-- 11B = (XXX, 11Z)
-- 11Z = (11B, XXX)
-- 22A = (22B, XXX)
-- 22B = (22C, 22C)
-- 22C = (22Z, 22Z)
-- 22Z = (22B, 22B)
-- XXX = (XXX, XXX)
-- Here, there are two starting nodes, 11A and 22A (because they both end with A). As you follow each left/right instruction, use
-- that instruction to simultaneously navigate away from both nodes you're currently on.
-- Repeat this process until all of the nodes you're currently on end with Z.
-- (If only some of the nodes you're on end with Z, they act like any other node and you continue as normal.)
-- In this example, you would proceed as follows:

-- Step 0: You are at 11A and 22A.
-- Step 1: You choose all of the left paths, leading you to 11B and 22B.
-- Step 2: You choose all of the right paths, leading you to 11Z and 22C.
-- Step 3: You choose all of the left paths, leading you to 11B and 22Z.
-- Step 4: You choose all of the right paths, leading you to 11Z and 22B.
-- Step 5: You choose all of the left paths, leading you to 11B and 22C.
-- Step 6: You choose all of the right paths, leading you to 11Z and 22Z.
-- So, in this example, you end up entirely on nodes that end in Z after 6 steps.

-- Simultaneously start on every node that ends with A. How many steps does it take before you're only on nodes that end with Z?

import Data.List (sort, sortBy)
import Data.Map (Map, findWithDefault, fromList, toList)

main = interact mainFunc

mainFunc :: String -> String
mainFunc input =
  let instructions = head $ lines input
      locationLines = drop 2 $ lines input
      locationMap = loadLocationMap locationLines
      allLocations = [k | (k, v) <- toList locationMap]
      startLocations = filter isStartNode allLocations
      steps = [walkToMultipleFinishPoints x locationMap (cycle instructions) 0 1 | x <- startLocations]
      diffs = [differences x | x <- steps]
      -- ok, they all loop but at different speeds..
      -- we can iterate in loop sizes of one of them and just check if that would have been the end of a loop for all the others too
      -- iterating the largest number would be favourite
      loopIntervals = sortBy (flip compare) [head x | x <- steps]
      result = findLoopIntersect loopIntervals
   in "Instructions: "
        ++ show instructions
        ++ "\nlocationMap:\n"
        ++ show locationMap
        ++ "\nstartLocations: "
        ++ show startLocations
        ++ "\nSteps to end: "
        ++ show steps
        ++ "\nDiffs in steps to end: "
        ++ show diffs
        ++ "\nloopIntervals: "
        ++ show loopIntervals
        ++ "\nresult: "
        ++ show result
        ++ "\n"

-- 16342438708751

findLoopIntersect :: [Int] -> Int
findLoopIntersect (x : xs) = foldr lcm x xs

isStartNode :: String -> Bool
isStartNode s = 'A' == last s

isEndNode :: String -> Bool
isEndNode s = 'Z' == last s

walkToMultipleFinishPoints :: String -> Map String (String, String) -> String -> Int -> Int -> [Int]
walkToMultipleFinishPoints _ _ _ _ 0 = []
walkToMultipleFinishPoints current locationMap instructions step remainingFinishPoints =
  let (left, right) = findWithDefault ("???", "???") current locationMap
      nextstep = if instructions !! step == 'L' then left else right
   in if isEndNode nextstep
        then step + 1 : walkToMultipleFinishPoints nextstep locationMap instructions (step + 1) (remainingFinishPoints - 1)
        else walkToMultipleFinishPoints nextstep locationMap instructions (step + 1) remainingFinishPoints

walkToEnd :: [String] -> Map String (String, String) -> String -> Int -> Int
walkToEnd current locationMap instructions step =
  let nextSteps = [findWithDefault ("???", "!!!") x locationMap | x <- current]
      nextLocations = if instructions !! step == 'L' then [fst x | x <- nextSteps] else [snd x | x <- nextSteps]
      endLocations = filter isEndNode nextLocations
   in if length endLocations == length nextLocations then (step + 1) else walkToEnd nextLocations locationMap instructions (step + 1)

loadLocationMap :: [String] -> Map String (String, String)
loadLocationMap xs =
  let locationList = [locationLineToListItem x | x <- xs]
   in fromList locationList

locationLineToListItem :: String -> (String, (String, String))
locationLineToListItem s =
  let parts = splitOn' '=' s
      a = cleanup $ head parts
      lrPart = parts !! 1
      directionParts = splitOn' ',' lrPart
      b = cleanup $ head directionParts
      c = cleanup $ directionParts !! 1
   in (a, (b, c))

cleanup :: String -> String
cleanup s =
  let cleaner = filter (/= ' ') . filter (/= '(') . filter (/= ')')
   in cleaner s

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

differences :: (Num a) => [a] -> [a]
differences [x] = []
differences (x : x2 : xs) = (x2 - x) : differences (x2 : xs)
