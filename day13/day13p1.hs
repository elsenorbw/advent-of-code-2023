-- --- Day 13: Point of Incidence ---
-- With your help, the hot springs team locates an appropriate spring which launches you neatly and precisely up to the edge of Lava Island.

-- There's just one problem: you don't see any lava.

-- You do see a lot of ash and igneous rock; there are even what look like gray mountains scattered around. After a while, you make your way to
-- a nearby cluster of mountains only to discover that the valley between them is completely full of large mirrors. Most of the mirrors seem to
-- be aligned in a consistent way; perhaps you should head in that direction?

-- As you move through the valley of mirrors, you find that several of them have fallen from the large metal frames keeping them in place.
-- The mirrors are extremely flat and shiny, and many of the fallen mirrors have lodged into the ash at strange angles.
-- Because the terrain is all one color, it's hard to tell where it's safe to walk or where you're about to run into a mirror.

-- You note down the patterns of ash (.) and rocks (#) that you see as you walk (your puzzle input); perhaps by carefully analyzing these patterns,
-- you can figure out where the mirrors are!

-- For example:

-- #.##..##.
-- ..#.##.#.
-- ##......#
-- ##......#
-- ..#.##.#.
-- ..##..##.
-- #.#.##.#.

-- #...##..#
-- #....#..#
-- ..##..###
-- #####.##.
-- #####.##.
-- ..##..###
-- #....#..#
-- To find the reflection in each pattern, you need to find a perfect reflection across either a horizontal line between two rows or
-- across a vertical line between two columns.

-- In the first pattern, the reflection is across a vertical line between two columns; arrows on each of the two columns point at the
-- line between the columns:

-- 123456789
--     ><
-- #.##..##.
-- ..#.##.#.
-- ##......#
-- ##......#
-- ..#.##.#.
-- ..##..##.
-- #.#.##.#.
--     ><
-- 123456789
-- In this pattern, the line of reflection is the vertical line between columns 5 and 6. Because the vertical line is not perfectly in
-- the middle of the pattern, part of the pattern (column 1) has nowhere to reflect onto and can be ignored; every other column has a
-- reflected column within the pattern and must match exactly: column 2 matches column 9, column 3 matches 8, 4 matches 7, and 5 matches 6.

-- The second pattern reflects across a horizontal line instead:

-- 1 #...##..# 1
-- 2 #....#..# 2
-- 3 ..##..### 3
-- 4v#####.##.v4
-- 5^#####.##.^5
-- 6 ..##..### 6
-- 7 #....#..# 7
-- This pattern reflects across the horizontal line between rows 4 and 5. Row 1 would reflect with a hypothetical row 8, but since that's not
-- in the pattern, row 1 doesn't need to match anything. The remaining rows match: row 2 matches row 7, row 3 matches row 6, and row 4 matches row 5.

-- To summarize your pattern notes, add up the number of columns to the left of each vertical line of reflection; to that, also add 100 multiplied
-- by the number of rows above each horizontal line of reflection.

-- In the above example, the first pattern's vertical line has 5 columns to its left and the second pattern's horizontal
-- line has 4 rows above it, a total of 405.

-- Find the line of reflection in each of the patterns in your notes. What number do you get after summarizing all of your notes?

data CaveMap = CaveMap
  { rows :: [String],
    columns :: [String],
    maxX :: Int,
    maxY :: Int
  }
  deriving (Show, Eq)

main = interact mainFunc

mainFunc :: String -> String
mainFunc input =
  let caves = loadCaveMaps input
      horizontalMatches = [findMirrors 1 (rows x) | x <- caves]
      verticalMatches = [findMirrors 1 (columns x) | x <- caves]
      result = 100 * sum horizontalMatches + sum verticalMatches
   in "\n\nCaves:\n"
        ++ printCaves caves
        ++ "\nHorizontal:"
        ++ show horizontalMatches
        ++ "\nVertical:"
        ++ show verticalMatches
        ++ "\nResult: "
        ++ show result
        ++ "\n\n"

findMirrors :: Int -> [String] -> Int
findMirrors chopPoint xs
  | chopPoint >= length xs = 0
  | otherwise =
      let a = reverse $ take chopPoint xs
          b = drop chopPoint xs
          matcher = zip a b
          matches = [a == b | (a, b) <- matcher]
       in if and matches then chopPoint else findMirrors (chopPoint + 1) xs

printCaves :: [CaveMap] -> String
printCaves caves = concat [printOneCave c | c <- caves]

printOneCave :: CaveMap -> String
printOneCave cave =
  "\nCave :"
    ++ show (maxX cave + 1)
    ++ ","
    ++ show (maxY cave + 1)
    ++ "\n"
    ++ "Rows:\n"
    ++ concat [s ++ "\n" | s <- rows cave]
    ++ "\nCols:\n"
    ++ concat [s ++ "\n" | s <- columns cave]
    ++ "\n"

loadCaveMaps :: String -> [CaveMap]
loadCaveMaps input =
  let caveInputs = splitOn' "" $ lines input
      caves = [readOneCave x | x <- caveInputs]
   in caves

readOneCave :: [String] -> CaveMap
readOneCave theLines =
  let theRows = theLines
      x = length (head theLines) - 1
      y = length theLines - 1
      theColumns = [[theLines !! row !! col | row <- [0 .. y]] | col <- [0 .. x]]
   in CaveMap theRows theColumns x y

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
