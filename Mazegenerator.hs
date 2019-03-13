module Mazegenerator where

import Grid
import System.Random
import Test.HUnit
import Data.List
import Debug.Trace
import Data.Array.IArray

-- The position of a Cell in coordinates.
type Position   = (Int,Int)
-- Stores the list with Positions that the mazegenerator have visited. 
type Visited    = [Position]
-- A List with a Positions neighbouring Positions.
type Neighbours = [Position]
-- A List with random numbers between 1-3.
type RandomList = [Int]


{-stuck grid xs
  Checks if all neighbours of a Cell have been visited or not.

  PRE: elements in xs is valid Positions of Cells found in grid
  RETURNS: True if all neighbours Cells in grid have been visited, otherwise False.
  VARIANT: length of (x:xs)
  EXAMPLE: stuck (array (1,2) [(1,[C (1,1) (True,True,True,True) [(2,1),(1,2)] False,C (2,1) (True,True,True,True) [(1,1),(2,2)] False]),(2,[C (1,2) (True,True,True,True) [(2,2),(1,1)] False,C (2,2) (True,True,True,True) [(1,2),(2,1)] False])]) [(1,2),(2,1)] = True
           stuck (array (1,2) [(1,[C (1,1) (True,True,True,True) [(2,1),(1,2)] False, C (2,1) (True,True,True,True) [(1,1),(2,2)] True]),(2,[C (1,2) (True,True,True,True) [(2,2),(1,1)] False,C (2,2) (True,True,True,True) [(1,2),(2,1)] False])]) [(1,2),(2,1)] = False
  
-}
stuck :: Grid -> Neighbours -> Bool
stuck grid [] = True
stuck grid (x:xs)
  | visited   = stuck grid xs
  | otherwise = False
  where
    visited = getVisited $ getCell grid x

{- randomNeighbour grid n rl
   Takes an unvisited neighbour out of the neighbours list. which neighbour is given by the elements in the provided randomlist.

   PRE: n contains valid positions of Cells in grid.
   RETURNS: (a position picked out of n at an index taken from rl, rl without the Ints used to get the position) 
   VARIANT: length of n
   EXAMPLE: grid [(1,2),(2,1)] [4,3,0,2,3,1,3] = ((1,2), [2,3,1,3])
            grid [(1,2),(2,1)] [4,3,1,2,3,0,3] = ((2,1), [2,3,0,3])
-}
randomNeighbour :: Grid -> Neighbours -> RandomList ->(Position,RandomList)
randomNeighbour grid n rl
  | visited   = randomNeighbour grid nl nrl
  | otherwise = (ranNeighbour,nrl)
  where
    (ranNeighbour, nrl) = pickElement n rl 
    visited             = getVisited $ getCell grid ranNeighbour
    nl = remove ranNeighbour n

{-pickElement xs rs
  recurse through  a list of "random numbers" untill a number is found so that the element at the index of that number in the neighbours list can be choosen.

  PRE: rs has atleast one element with a value lower than the length of xs. rs is not empty.
  RETURNS: (a position picked out of xs at an index taken from rs, rs without the Ints used to get the position)
  VARIANT: length of rs
  EXAMPLE: pickElement [(1,2),(2,1)] [4,3,0,2,3,1,3] = ((1,2), [2,3,1,3])
           pickElement [(1,2),(2,1)] [4,3,1,2,3,0,3] = ((2,1), [2,3,0,3])
          
-}
pickElement :: Neighbours -> RandomList -> (Position,RandomList)
pickElement xs (r:rs)
  | length xs > r  = ((xs !! r),rs)
  | otherwise      = pickElement xs rs

{- mazeGenerator grid p rl vis
   modifies the walls in a grid so it becomes a solveable maze. if starting in position (1,1) in the grid, there will be a path through open walls to every other Cell in the grid.

   PRE: p is a valid position in grid. vis is a empty list.
   RETURNS: a copy of grid, with the walls and visited status of each cell in grid modified depending on rl.
   VARIANT: amount of unvisited cells in grid.
   EXAMPLE: mazeGenerator (array (1,2) [(1,[C (1,1) (True,True,True,True) [(2,1),(1,2)] False,C (2,1) (True,True,True,True) [(1,1),(2,2)] False]),(2,[C (1,2) (True,True,True,True) [(2,2),(1,1)] False,C (2,2) (True,True,True,True) [(1,2),(2,1)] False])]) (1,1) [0,2,3,2,1,3,2,1,0,2,1,2,3,2,1,1,2,3] [] = array (1,2) [(1,[C (1,1) (True,False,False,True) [(2,1),(1,2)] True,C (2,1) (False,True,False,True) [(1,1),(2,2)] True]),(2,[C (1,2) (True,False,True,False) [(2,2),(1,1)] True,C (2,2) (False,True,True,False) [(1,2),(2,1)] True])]
-}
mazeGenerator :: Grid -> Position -> RandomList -> Visited -> Grid
mazeGenerator grid p rl []
  | vis == True = grid
  | stuck grid neigh  = grid
  | otherwise = mazeGenerator newc npos nrl nvl
   where
    (pos,wall,neigh,vis) = getCellInfo $ getCell grid p
    (newc, npos, nrl, nvl) = mazeGenerator' grid p neigh rl []
mazeGenerator c p [] vl = c
mazeGenerator c p rl (v:vl)
  | stuck c neigh  = mazeGenerator c v rl vl
  | otherwise = mazeGenerator newc npos nrl nvl
  where
    (pos,wall,neigh,vis) = getCellInfo $ getCell c p
    (newc, npos, nrl, nvl) = mazeGenerator' c p neigh rl (v:vl)

{- mazeGenerator' grid p neigh rl vis
   modifies the walls in a grid. if starting in position (1,1) in the grid, there will be a path through open walls to every other cell in the grid.

   PRE: p is a valid position in grid. n is a list of valid neighbour positions to p.
   RETURNS: (newc, pos, nrl, nvl) where
            newc is a copy of grid, with the walls and visited status of the Cell with position p in grid modified depending on rl.
            pos is a position picked out of neigh at an index taken from rl
            nrl is rl with all used elements removed
            nvl is (p:vis)
   VARIANT: length rl
   EXAMPLE: mazeGenerator (array (1,2) [(1,[C (1,1) (True,True,True,True) [(2,1),(1,2)] False,C (2,1) (True,True,True,True) [(1,1),(2,2)] False]),(2,[C (1,2) (True,True,True,True) [(2,2),(1,1)] False,C (2,2) (True,True,True,True) [(1,2),(2,1)] False])]) (1,1) [(1,2),(2,1)] [0,2,3,2,1,3,2,1,0,2,1,2,3,2,1,1,2,3] [] = ((array (1,2) [(1,[C (1,1) (True,True,False,True) [(2,1),(1,2)] True,C (2,1) (True,True,True,True) [(1,1),(2,2)] False]),(2,[C (1,2) (True,True,True,False) [(2,2),(1,1)] True,C (2,2) (True,True,True,True) [(1,2),(2,1)] False])]), (1,2), [2,3,2,1,3,2,1,0,2,1,2,3,2,1,1,2,3], [(1,1)])
-}
mazeGenerator' :: Grid -> Position -> Neighbours -> RandomList -> Visited -> (Grid, Position, RandomList, Visited)
mazeGenerator' grid p n rl vl = (newc, pos, nrl, (p:vl))
  where
    (pos,nrl) = randomNeighbour grid n rl
    tempc  = setWall grid p (wallDirection p pos)
    tempc' = setWall tempc pos (wallDirection pos p)
    newc   = setVisited tempc' pos True

{- wallDirection p@(x,y) p'@(x',y')
   Checks in what direction p' is located in relation to p.

   RETURNS: a String that is given by the differends between (x,y) and (x',y')
   EXAMPLE: wallDirection (1,1) (1,2) = "n"
            wallDirection (1,2) (1,1) = "s"
-}
wallDirection :: Position -> Position -> String
wallDirection (x,y) (x',y')
  | x > x' = "w"
  | x < x' = "e"
  | y < y' = "n"
  | y > y' = "s"

-- removes an element from a list.
remove element list = filter (\e -> e/=element) list

{-genRandom x
  use the provided number as seed to generate a list with numbers between 1-3.

  RETURNS: a list with Ints generated with x as a seed
  EXAMPLE: genRandom 2 = [1,3,2,0.....]
-}
genRandom :: Int -> [Int]
genRandom x = take 50000 (randomRs (0,3) (mkStdGen x))

{- countCells g max
   counts the cells in a grid.

   PRE: max is the number of keys in g
   RETURNS: The amount of Cells in g
   EXAMPLE: countCells (array (1,2) [(1,[C (1,1) (True,True,True,True) [(2,1),(1,2)] False,C (2,1) (True,True,True,True) [(1,1),(2,2)] False]),(2,[C (1,2) (True,True,True,True) [(2,2),(1,1)] False,C (2,2) (True,True,True,True) [(1,2),(2,1)] False])]) 2 = 4
-}
countCells :: Grid -> Int -> Int
countCells grid max
  | max == 0  = 0
  | otherwise = length (grid ! max) + countCells grid (max-1) 


---------------------------------------------------------------------------------------------------------------
                                             -- TEST CASES --
---------------------------------------------------------------------------------------------------------------
-- Tests for stuck --------------------------------------------------------------

testStuck = runTestTT $ TestList [testStuck1, testStuck2]
testStuck1 = TestCase $ assertEqual "Testing function stuck" False (stuck (createGrid (2,2)) [(2,1), (1,2)])
testStuck2 = TestCase $ assertEqual "Testing function stuck" True (stuck (mazeGenerator (createGrid (2,2)) (1,1) (genRandom 2) []) [(2,1), (1,2)])

-- Tests for randomNeighbours ---------------------------------------------------

testRandomNeighbour = runTestTT $ TestList [testRandomNeighbour1, testRandomNeighbour2, testRandomNeighbour3]

testRandomNeighbour1 = TestCase $ assertEqual "Testing fst randomNeighbours" (1,2) (fst $ (randomNeighbour (createGrid (5,5)) [(1,2),(3,2),(2,1),(2,3)] [0,2,3,1,2]))
testRandomNeighbour2 = TestCase $ assertEqual "Testing snd randomNeighbours" [2,3,1,2] (snd $ (randomNeighbour (createGrid (5,5)) [(1,2),(3,2),(2,1),(2,3)] [0,2,3,1,2]))
testRandomNeighbour3 = TestCase $ assertEqual "Testing randomNeighbours" ((2,3),[1,2]) (randomNeighbour (createGrid (5,5)) [(1,2),(3,2),(2,1),(2,3)] [3,1,2])


-- Tests for wallDirection ------------------------------------------------------

testWallDirection = runTestTT $ TestList [testWallDirection1, testWallDirection2, testWallDirection3, testWallDirection4]

testWallDirection1 = TestCase $ assertEqual "Testing wallDirection e" "e" (wallDirection (1,1) (2,1))
testWallDirection2 = TestCase $ assertEqual "Testing wallDirection w" "w" (wallDirection (2,1) (1,1))
testWallDirection3 = TestCase $ assertEqual "Testing wallDirection s" "s" (wallDirection (1,2) (1,1))
testWallDirection4 = TestCase $ assertEqual "Testing wallDirection n" "n" (wallDirection (1,1) (1,2))


-- Tests for pickElement --------------------------------------------------------

testPickElement = runTestTT $ TestList [testPickElement1, testPickElement2]


testPickElement1 = TestCase $ assertEqual "Testing pickElement" ((1,1),[1,2]) (pickElement [(1,1),(3,1),(1,2)] [3,0,1,2])
testPickElement2 = TestCase $ assertEqual "Testing pickElement" ((3,1),[2]) (pickElement [(1,1),(3,1),(1,2)] [3,3,1,2])

-- Test for mazeGenerator -------------------------------------------------------
testMazeGenerator = TestCase $ assertEqual "Testing that all cells remains in the grid after it run through the mazegenerator" 100 (countCells (mazeGenerator (createGrid (10,10)) (1,1) (genRandom 33) []) 10)
