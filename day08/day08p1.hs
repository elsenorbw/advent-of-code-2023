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

import Data.Map (Map, findWithDefault, fromList)
import Language.Haskell.TH (location)

main = interact mainFunc

mainFunc :: String -> String
mainFunc input =
  let instructions = head $ lines input
      locationLines = drop 2 $ lines input
      locations = loadLocationMap locationLines
      steps = walkToEnd "AAA" locations (cycle instructions) 0
   in "Instructions: "
        ++ show instructions
        ++ "\nlocations:\n"
        ++ show locations
        ++ "\nSteps to end: "
        ++ show steps
        ++ "\n"

walkToEnd :: String -> Map String (String, String) -> String -> Int -> Int
walkToEnd "ZZZ" _ _ x = x
walkToEnd current locationMap instructions step =
  let (left, right) = findWithDefault ("???", "???") current locationMap
      nextstep = if instructions !! step == 'L' then left else right
   in walkToEnd nextstep locationMap instructions (step + 1)

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
