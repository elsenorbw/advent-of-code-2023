-- --- Day 19: Aplenty ---
-- The Elves of Gear Island are thankful for your help and send you on your way. They even have a hang glider that someone stole from Desert Island;
-- since you're already going that direction, it would help them a lot if you would use it to get down there and return it to them.

-- As you reach the bottom of the relentless avalanche of machine parts, you discover that they're already forming a formidable heap. Don't worry, though -
-- a group of Elves is already here organizing the parts, and they have a system.

-- To start, each part is rated in each of four categories:

-- x: Extremely cool looking
-- m: Musical (it makes a noise when you hit it)
-- a: Aerodynamic
-- s: Shiny
-- Then, each part is sent through a series of workflows that will ultimately accept or reject the part. Each workflow has a name and contains a list of rules;
-- each rule specifies a condition and where to send the part if the condition is true. The first rule that matches the part being considered is applied immediately,
--    and the part moves on to the destination described by the rule. (The last rule in each workflow has no condition and always applies if reached.)

-- Consider the workflow ex{x>10:one,m<20:two,a>30:R,A}. This workflow is named ex and contains four rules. If workflow ex were considering a specific part, it would perform the following steps in order:

-- Rule "x>10:one": If the part's x is more than 10, send the part to the workflow named one.
-- Rule "m<20:two": Otherwise, if the part's m is less than 20, send the part to the workflow named two.
-- Rule "a>30:R": Otherwise, if the part's a is more than 30, the part is immediately rejected (R).
-- Rule "A": Otherwise, because no other rules matched the part, the part is immediately accepted (A).
-- If a part is sent to another workflow, it immediately switches to the start of that workflow instead and never returns.
-- If a part is accepted (sent to A) or rejected (sent to R), the part immediately stops any further processing.

-- The system works, but it's not keeping up with the torrent of weird metal shapes. The Elves ask if you can help sort a few parts and give you the
--    list of workflows and some part ratings (your puzzle input). For example:

-- px{a<2006:qkq,m>2090:A,rfg}
-- pv{a>1716:R,A}
-- lnx{m>1548:A,A}
-- rfg{s<537:gd,x>2440:R,A}
-- qs{s>3448:A,lnx}
-- qkq{x<1416:A,crn}
-- crn{x>2662:A,R}
-- in{s<1351:px,qqz}
-- qqz{s>2770:qs,m<1801:hdj,R}
-- gd{a>3333:R,R}
-- hdj{m>838:A,pv}

-- {x=787,m=2655,a=1222,s=2876}
-- {x=1679,m=44,a=2067,s=496}
-- {x=2036,m=264,a=79,s=2244}
-- {x=2461,m=1339,a=466,s=291}
-- {x=2127,m=1623,a=2188,s=1013}
-- The workflows are listed first, followed by a blank line, then the ratings of the parts the Elves would like you to sort. All parts begin in the workflow named in.
-- In this example, the five listed parts go through the following workflows:

-- {x=787,m=2655,a=1222,s=2876}: in -> qqz -> qs -> lnx -> A
-- {x=1679,m=44,a=2067,s=496}: in -> px -> rfg -> gd -> R
-- {x=2036,m=264,a=79,s=2244}: in -> qqz -> hdj -> pv -> A
-- {x=2461,m=1339,a=466,s=291}: in -> px -> qkq -> crn -> R
-- {x=2127,m=1623,a=2188,s=1013}: in -> px -> rfg -> A
-- Ultimately, three parts are accepted. Adding up the x, m, a, and s rating for each of the accepted parts gives 7540 for the part with x=787, 4623 for the part with x=2036, and 6951
-- for the part with x=2127. Adding all of the ratings for all of the accepted parts gives the sum total of 19114.

-- Sort through all of the parts you've been given; what do you get if you add together all of the rating numbers for all of the parts that ultimately get accepted?

-- To begin, get your puzzle input.

import Data.Map (Map, fromList, lookup)
import Data.Maybe (catMaybes, fromJust)

data LogicCase = LessThan | GreaterThan | Immediate deriving (Show)

data Condition = Condition
  { targetField :: Char,
    condition :: LogicCase,
    compareValue :: Int,
    jumpLocation :: String
  }
  deriving (Show)

data Rule = Rule
  { ruleName :: String,
    conditions :: [Condition]
  }
  deriving (Show)

type Ruleset = Map String Rule

data Part = Part
  { theX :: Int,
    theM :: Int,
    theA :: Int,
    theS :: Int
  }
  deriving (Show, Eq)

main = interact mainFunc

mainFunc :: String -> String
mainFunc input =
  let (ruleStr, partStr) = getStringParts input
      ruleset = fromList $ loadRuleset ruleStr
      partList = loadPartList partStr
      acceptedParts = filter (isPartAccepted ruleset "in") partList
      ratings = [ratingFor p | p <- acceptedParts]
      result = sum ratings
   in "RuleString:\n"
        ++ show ruleStr
        ++ "\nRuleset:\n"
        ++ show ruleset
        ++ "\nPartString:\n"
        ++ show partStr
        ++ "\nPartList:\n"
        ++ show partList
        ++ "\nAccepted Part:\n"
        ++ show acceptedParts
        ++ "\nRatings:\n"
        ++ show ratings
        ++ "\nResult:\n"
        ++ show result
        ++ "\n"

ratingFor :: Part -> Int
ratingFor p = theX p + theM p + theA p + theS p

isPartAccepted :: Ruleset -> String -> Part -> Bool
isPartAccepted _ "A" _ = True
isPartAccepted _ "R" _ = False
isPartAccepted ruleset thisRuleName part =
  let thisRule = fromJust $ Data.Map.lookup thisRuleName ruleset
      conds = conditions thisRule
      conditionResults = [evalCondition x part | x <- conds]
      firstResult = head $ catMaybes conditionResults
   in isPartAccepted ruleset firstResult part

evalCondition :: Condition -> Part -> Maybe String
evalCondition cond part =
  let tgtField = targetField cond
      condRule = condition cond
      ruleVal = compareValue cond
      jumpLoc = jumpLocation cond
      partVal = getPartValue tgtField part
      ruleResult = evalRule condRule partVal ruleVal
   in if ruleResult then Just jumpLoc else Nothing

evalRule :: LogicCase -> Int -> Int -> Bool
evalRule Immediate _ _ = True
evalRule LessThan partVal ruleVal = partVal < ruleVal
evalRule GreaterThan partVal ruleVal = partVal > ruleVal

getPartValue :: Char -> Part -> Int
getPartValue fv part
  | fv == 'x' = theX part
  | fv == 'm' = theM part
  | fv == 'a' = theA part
  | fv == 's' = theS part
  | otherwise = error "Hurmph!"

loadPartList :: [String] -> [Part]
loadPartList [] = []
loadPartList (x : xs) =
  let cleanStr = (filter (/= '{') . filter (/= '}')) x
      theVals = splitOn' ',' cleanStr
      startPart = Part 0 0 0 0
      endPart = foldl applyOneValue startPart theVals
   in endPart : loadPartList xs

applyOneValue :: Part -> String -> Part
applyOneValue part (c : x : vs) =
  let intVal = read vs :: Int
   in applyTheValue part c intVal

applyTheValue :: Part -> Char -> Int -> Part
applyTheValue part fld val
  | fld == 'x' = Part val (theM part) (theA part) (theS part)
  | fld == 'm' = Part (theX part) val (theA part) (theS part)
  | fld == 'a' = Part (theX part) (theM part) val (theS part)
  | fld == 's' = Part (theX part) (theM part) (theA part) val
  | otherwise = error "Wut ?"

loadRuleset :: [String] -> [(String, Rule)]
loadRuleset [] = []
loadRuleset (x : xs) =
  let bits = splitOn' '{' x
      theName = head bits
      conditionStr = filter (/= '}') $ bits !! 1
      conditionBits = splitOn' ',' conditionStr
      theConditions = [readOneCondition x | x <- conditionBits]
      theRule = Rule theName theConditions
   in (theName, theRule) : loadRuleset xs

readOneCondition :: String -> Condition
readOneCondition s
  | ':' `notElem` s = Condition 'x' Immediate (-1) s
  | otherwise =
      let parts = splitOn' ':' s
          theDestination = parts !! 1
          (a : b : c) = head parts
          theField = a
          theCompareRule = if b == '<' then LessThan else GreaterThan
          theCompareValue = read c :: Int
       in Condition theField theCompareRule theCompareValue theDestination

getStringParts :: String -> ([String], [String])
getStringParts input =
  let theLines = lines input
      blocks = splitOn' "" theLines
   in (head blocks, blocks !! 1)

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
