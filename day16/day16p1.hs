-- --- Day 16: The Floor Will Be Lava ---
-- With the beam of light completely focused somewhere, the reindeer leads you deeper still into the Lava Production Facility.
-- At some point, you realize that the steel facility walls have been replaced with cave, and the doorways are just cave, and the
-- floor is cave, and you're pretty sure this is actually just a giant cave.

-- Finally, as you approach what must be the heart of the mountain, you see a bright light in a cavern up ahead. There, you discover that
-- the beam of light you so carefully focused is emerging from the cavern wall closest to the facility and pouring all of its energy into a
-- contraption on the opposite side.

-- Upon closer inspection, the contraption appears to be a flat, two-dimensional square grid containing empty space (.), mirrors (/ and \), and splitters (| and -).

-- The contraption is aligned so that most of the beam bounces around the grid, but each tile on the grid converts some of the beam's light into heat to melt the rock in the cavern.

-- You note the layout of the contraption (your puzzle input). For example:

--  .|...\....
--  |.-.\.....
--  .....|-...
--  ........|.
--  ..........
--  .........\
--  ..../.\\..
--  .-.-/..|..
--  .|....-|.\
--  ..//.|....
--  The beam enters in the top-left corner from the left and heading to the right. Then, its behavior depends on what it encounters as it moves:

-- If the beam encounters empty space (.), it continues in the same direction.
-- If the beam encounters a mirror (/ or \), the beam is reflected 90 degrees depending on the angle of the mirror. For instance, a rightward-moving beam that
-- encounters a / mirror would continue upward in the mirror's column, while a rightward-moving beam that encounters a \ mirror would continue downward from the mirror's column.
-- If the beam encounters the pointy end of a splitter (| or -), the beam passes through the splitter as if the splitter were empty space. For instance, a rightward-moving beam
--    that encounters a - splitter would continue in the same direction.
-- If the beam encounters the flat side of a splitter (| or -), the beam is split into two beams going in each of the two directions the splitter's pointy ends are pointing.
-- For instance, a rightward-moving beam that encounters a | splitter would split into two beams: one that continues upward from the splitter's column and one that continues downward
-- from the splitter's column.
-- Beams do not interact with other beams; a tile can have many beams passing through it at the same time. A tile is energized if that tile has at least one beam pass through it, reflect in it, or split in it.

-- In the above example, here is how the beam of light bounces around the contraption:

--  >|<<<\....
--  |v-.\^....
--  .v...|->>>
--  .v...v^.|.
--  .v...v^...
--  .v...v^..\
--  .v../2\\..
--  <->-/vv|..
--  .|<<<2-|.\
--  .v//.|.v..
--  Beams are only shown on empty tiles; arrows indicate the direction of the beams. If a tile contains beams moving in multiple directions, the number of distinct directions is shown instead.
--  Here is the same diagram but instead only showing whether a tile is energized (#) or not (.):

-- ######....
-- .#...#....
-- .#...#####
-- .#...##...
-- .#...##...
-- .#...##...
-- .#..####..
-- ########..
-- .#######..
-- .#...#.#..
-- Ultimately, in this example, 46 tiles become energized.

-- The light isn't energizing enough tiles to produce lava; to debug the contraption, you need to start by analyzing the current situation. With the beam starting in the top-left heading right,
-- how many tiles end up being energized?

-- ok, so we need to run all the beams until we reach the end of the map or they're somewhere that they have already been and are travelling in the same direction.
-- should be fun.
import Data.Map (Map, empty, insert, lookup)
import Data.Set qualified as S (Set, empty, insert, size, toList)

data MirrorType = Vertical | Horizontal | Backslash | Slash deriving (Show, Eq)

data Direction = DirLeft | DirRight | DirUp | DirDown deriving (Show, Eq, Ord)

data Location = Location
  { theX :: Int,
    theY :: Int
  }
  deriving (Show, Eq, Ord)

data MirrorZone = MirrorZone
  { maxX :: Int,
    maxY :: Int,
    mirrors :: Map Location MirrorType
  }
  deriving (Show)

newtype HistoricLocations = HistoricLocations
  { visited :: S.Set (Location, Direction)
  }
  deriving (Show)

main = interact mainFunc

mainFunc :: String -> String
mainFunc input =
  let zone = loadMirrorZone input
      starters = [(Location 0 0, DirRight)]
      visitedSoFar = HistoricLocations S.empty
      visitedList = fireTheLaser zone starters visitedSoFar
      visitedLocations = foldl shoveIntoSet S.empty [l | (l, _) <- S.toList (visited visitedList)]
      result = S.size visitedLocations
   in "\n"
        ++ show zone
        ++ "\nFinal visited:\n"
        ++ show visitedList
        ++ "\nResult : "
        ++ show result
        ++ "\n"

fireTheLaser :: MirrorZone -> [(Location, Direction)] -> HistoricLocations -> HistoricLocations
fireTheLaser zone [] locations = locations
fireTheLaser zone lasers locations =
  let existingSet = visited locations
      remainingLasers = filter (`notElem` visited locations) lasers
      -- add remaining laser positions to the list
      nextLocations = foldl shoveIntoSet existingSet remainingLasers
      -- generate the output lasers after they hit the mirrors
      turnedLasers = concat [mirrorBounceLaser zone l | l <- remainingLasers]
      -- increment all the turned lasers
      movedLasers = [moveLaser l | l <- turnedLasers]
      -- remove the out of bounds lasers
      validLasers = filter (inBoundCheck zone) movedLasers
   in fireTheLaser zone validLasers (HistoricLocations nextLocations)

inBoundCheck :: MirrorZone -> (Location, Direction) -> Bool
inBoundCheck zone (Location x y, _) =
  let xValid = x >= 0 && x <= maxX zone
      yValid = y >= 0 && y <= maxY zone
   in xValid && yValid

moveLaser :: (Location, Direction) -> (Location, Direction)
moveLaser (Location x y, direction)
  | direction == DirDown = (Location x (y + 1), direction)
  | direction == DirUp = (Location x (y - 1), direction)
  | direction == DirLeft = (Location (x - 1) y, direction)
  | direction == DirRight = (Location (x + 1) y, direction)
  | otherwise = error "cant move a laser that way buddy"

lookupMirror :: MirrorZone -> Location -> Maybe MirrorType
lookupMirror zone location = Data.Map.lookup location (mirrors zone)

mirrorBounceLaser :: MirrorZone -> (Location, Direction) -> [(Location, Direction)]
mirrorBounceLaser zone (location, direction) =
  let mirrorAtLocation = lookupMirror zone location
   in generateBouncedLasers location direction mirrorAtLocation

generateBouncedLasers :: Location -> Direction -> Maybe MirrorType -> [(Location, Direction)]
-- no mirror, no change
generateBouncedLasers location direction Nothing = [(location, direction)]
generateBouncedLasers location direction mirror
  | mirror == Just Vertical && (direction == DirRight || direction == DirLeft) = [(location, DirUp), (location, DirDown)]
  | mirror == Just Vertical && (direction == DirUp || direction == DirDown) = [(location, direction)]
  | mirror == Just Horizontal && (direction == DirRight || direction == DirLeft) = [(location, direction)]
  | mirror == Just Horizontal && (direction == DirUp || direction == DirDown) = [(location, DirLeft), (location, DirRight)]
  | mirror == Just Slash && direction == DirUp = [(location, DirRight)]
  | mirror == Just Slash && direction == DirDown = [(location, DirLeft)]
  | mirror == Just Slash && direction == DirLeft = [(location, DirDown)]
  | mirror == Just Slash && direction == DirRight = [(location, DirUp)]
  | mirror == Just Backslash && direction == DirUp = [(location, DirLeft)]
  | mirror == Just Backslash && direction == DirDown = [(location, DirRight)]
  | mirror == Just Backslash && direction == DirLeft = [(location, DirUp)]
  | mirror == Just Backslash && direction == DirRight = [(location, DirDown)]
  | otherwise = error "Cannot bounce with this.."

shoveIntoSet :: (Ord a) => S.Set a -> a -> S.Set a
shoveIntoSet theSet newVal = S.insert newVal theSet

loadMirrorZone :: String -> MirrorZone
loadMirrorZone input =
  let theLines = lines input
      maxX = length (head theLines) - 1
      maxY = length theLines - 1
      startMirrors = Data.Map.empty
      charPairs = concat [[(Location x y, c) | (x, c) <- zip [0 ..] s] | (s, y) <- zip theLines [0 ..]]
      finalMirrors = foldl addMirrorsToMap startMirrors charPairs
   in MirrorZone maxX maxY finalMirrors

addMirrorsToMap :: Map Location MirrorType -> (Location, Char) -> Map Location MirrorType
addMirrorsToMap map (location, c)
  | c == '.' = map
  | c == '/' = insert location Slash map
  | c == '\\' = insert location Backslash map
  | c == '|' = insert location Vertical map
  | c == '-' = insert location Horizontal map
  | otherwise = error "Why would you do this ?"

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
