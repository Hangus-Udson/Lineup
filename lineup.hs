-- COMP30020 Assignment One
-- Author: Angus Hudson <ahudson1@student.unimelb.edu.au>
-- Student ID: 835808
-- Purpose: Implementing a structure and algorithms for a criminal guessing
-- game as defined in the Assignment 1 task sheet
--
-- This file contains the module Proj1, which is responsible for defining
-- types and functions responsible for the guessing component of a logical
-- guessing game that involves deducing a pair of criminals from eyewitness
-- feedback from lineups. The module uses a 'GameState' type that begins
-- with all possible pairs of criminals. In each feedback loop, a
-- 4-element 'result' tuple is produced, indicating matches in whole person,
-- height, hair and sex, this tuple is used to reduce the 'GameState'.
-- 'bestCandidate' then deduces the best choice for the next guess in terms
-- of expected candidates remaining after receiving it's 'result' tuple.
-- This continues until only 1 possible candidate remains in the 'GameState',
-- indicating the culprit pair has been identified.


module Proj1 (Person, parsePerson, height, hair, sex,
GameState, initialGuess, nextGuess, feedback) where

import Data.List

-- Define a type for a 'Person' or suspect, a person can be short or tall
-- (S|T), have blonde, red or dark hair (B|R|D) and be male or female (M|F)
data Person = Person Height Hair Sex
  deriving (Show, Eq)
data Height = S | T
  deriving (Show, Eq)
data Hair = B | R | D
  deriving (Show, Eq)
data Sex = M | F
  deriving (Show, Eq)

-- The 'GameState' stores a list of pairs of suspects still possible to
-- be the culprit pair based on results from each feedback loop from
-- other (incorrect) inspected lineups
type GameState = [[Person]]

-- Takes a three-character string and returns Just 'p', where 'p' is the
-- person specified by that string. If an invalid string, returns Nothing.
parsePerson :: String -> Maybe Person
parsePerson "SBM" = Just (Person S B M)
parsePerson "SRM" = Just (Person S R M)
parsePerson "SDM" = Just (Person S D M)
parsePerson "SBF" = Just (Person S B F)
parsePerson "SRF" = Just (Person S R F)
parsePerson "SDF" = Just (Person S D F)
parsePerson "TBM" = Just (Person T B M)
parsePerson "TRM" = Just (Person T R M)
parsePerson "TDM" = Just (Person T D M)
parsePerson "TBF" = Just (Person T B F)
parsePerson "TRF" = Just (Person T R F)
parsePerson "TDF" = Just (Person T D F)
parsePerson _ = Nothing

-- Get the height of a 'Person'
height :: Person -> Height
height (Person h _ _) = h

-- Get the hair of a 'Person'
hair :: Person -> Hair
hair (Person _ h _) = h

-- Get the sex of a 'Person'
sex :: Person -> Sex
sex (Person _ _ s) = s

-- Takes first a list of the true culprits and second a list of the suspects
-- in the lineup, and returns a quadruple of correct suspects,
-- heights, hair colours, and sexes, in that order. Note that correct
-- suspects are not included in the other calculation
feedback :: [Person] -> [Person] -> (Int,Int,Int,Int)
feedback crim sus = (length [x | x <- crim, y <- sus, x == y], same_height
  0 rcrim rsus, same_hair 0 rcrim rsus, same_sex 0 rcrim rsus)
    -- We use rcrim and rsus to remove correct suspects from both lists to
    -- avoid overlap
    where rcrim = [x | x <- crim, not (elem x sus)]
          rsus = [y | y <- sus, not (elem y crim)]

-- Takes first a counter value (for recursion), second a list of true culprits
-- and third a list of suspects in the lineup, and returns the number of
-- height matches
same_height :: Int -> [Person] -> [Person] -> Int
same_height count [] _ = count
same_height count (x:xs) sus = if (length matches > 0)
  then same_height (count+1) xs [s | s <- sus, s /= head matches]
  else same_height count xs sus
    where matches = [y | y <- sus, height x == height y]

-- Takes first a counter value (for recursion), second a list of true culprits
-- and third a list of suspects in the lineup, and returns the number of
-- hair matches
same_hair :: Int -> [Person] -> [Person] -> Int
same_hair count [] _ = count
same_hair count (x:xs) sus = if (length matches > 0)
  then same_hair (count+1) xs [s | s <- sus, s /= head matches]
  else same_hair count xs sus
    where matches = [y | y <- sus, hair x == hair y]

-- Takes first a counter value (for recursion), second a list of true culprits
-- and third a list of suspects in the lineup, and returns the number of
-- sex matches
same_sex :: Int -> [Person] -> [Person] -> Int
same_sex count [] _ = count
same_sex count (x:xs) sus = if (length matches > 0)
  then same_sex (count+1) xs [s | s <- sus, s /= head matches]
  else same_sex count xs sus
    where matches = [y | y <- sus, sex x == sex y]

-- Returns an initial lineup (set by default to a short blonde male and a
-- short red-haired female) and a 'GameState' containing all possible pair
-- lineups
initialGuess :: ([Person],GameState)
initialGuess = ([Person S B M, Person S R M], [[x,y] |
  x <- [Person a b c | a <- [S,T], b <- [B,R,D], c <- [M,F]], y <-
  [Person a b c | a <- [S,T], b <- [B,R,D], c <- [M,F]], x /= y])

-- Takes a pair of the previous guess and game state (as returned by
-- initialGuess and nextGuess), and the feedback to this guess as a quadruple
-- of correct suspects, correct height, correct hair colour, and correct sex,
-- and returns a pair of the next guess and updated game state
nextGuess :: ([Person],GameState) -> (Int,Int,Int,Int) -> ([Person],GameState)
-- Use 'bestCandidate' to deduce the best possible guess with given info
nextGuess (p, pstate) res = (bestCandidate newState, newState)
  -- 'newState' refers to the new 'gameState' that is formed after
  -- removing infeasible pairs based on lineup results from the previous
  -- iteration (p)
  where newState = [x | x <- pstate, (res == feedback x p) && (x /= p)]

-- Takes a list of possible culprit pairs and returns the 'best guess',
-- the lineup with the lowest expected number of possible lineups remaining
-- after receiving feedback for that lineup
bestCandidate :: [[Person]] -> [Person]
bestCandidate [x] = x
bestCandidate (x:xs)
    | ave < ave_min = x -- Have found a better candidate
    | otherwise = curr_min
    -- candidate_feedbacks: feedback for all pairs assuming 'x' is real pair
    -- min_feedbacks: feedback for all pairs assuming 'curr_min' is real pair
    -- curr_min: best candidate out of the rest of the list of candidate pairs
    -- ave: expected no. of remaining pairs upon using 'x' feedback
    -- ave_min: expected no. of remaining pairs upon using 'curr_min' feedback
    where candidate_feedbacks = [feedback x y | y <- (x:xs)]
          curr_min = bestCandidate xs
          min_feedbacks = [feedback curr_min y | y <- (x:xs)]
          ave = sum [length (filter (== x) candidate_feedbacks) `div` length
                candidate_feedbacks | x <- candidate_feedbacks]
          ave_min = sum [length (filter (== x) min_feedbacks) `div` length
                candidate_feedbacks | x <- min_feedbacks]
