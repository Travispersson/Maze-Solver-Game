module Pathfinder where 

import Grid
import Mazegenerator
import Test.HUnit


{- solver g (p,e) a
The main funktion of the pathfinding algorithm. It tries to go east/north/west/south (in that order) in order to find it's way from the position in the lowest left corner to the top right corner. 
PRE: g must be a valid grid and p must be the max position of g 
VARIANT: Amount of Positions needed to go through to get from p to e 
RETURNS: [The Positions gone thrugh to get from p to e]
EXAMPLE: solver (mazeGenerator (createGrid (5,5)) (1,1) maze1 []) ((1,1),(5,5)) [] = [(1,1),(2,1),(3,1),(4,1),(5,1),(5,2),(5,3),(5,4),(5,5)]
solver (mazeGenerator (createGrid (2,2)) (1,1) maze1 []) ((1,1),(2,2)) [] = [(1,1),(2,1),(2,2)]

-}
solver :: Grid  -> (Position, Position) -> [Position]-> [Position]
solver c p@(pos, end) acc
  | pos == (end) = reverse $ ((fst p) :acc)
solver c p@((1,1), end) acc
  | east == False = solver c ((2, 1),end) ((fst p):acc)
  | north == False = solver c ((1, 2), end) ((fst p) :acc)
      where
      (pos, walls, neig, vis) = getCellInfo $ getCell c (fst p)
      (west, east, north, south) = walls 
solver c p@((x,y), end) acc
  | east  == False && noBack (x+1, y) acc = solver c ((x+1, y),end) ((fst p):acc)
  | north == False && noBack (x, y+1) acc = solver c ((x, y+1),end) ((fst p):acc)
  | west  == False && noBack (x-1, y) acc = solver c ((x-1, y),end) ((fst p):acc)
  | south == False && noBack (x, y-1) acc = solver c ((x, y-1),end) ((fst p):acc)
  | otherwise = solver c (stuck, end) (stuck:acc) 
    where
     (pos, walls, neig, vis) = getCellInfo $ getCell c (fst p)
     (west, east, north, south) = walls
     stuck = backtrack c (allNeigs c acc)

{- Backtrack g [(p,n)] 
The funktion that's used when the algorithm gets "stuck" in the maze, i.e the algorithm gets to a position in the maze by going right were there's no way out. It generates a new posistion from where the algorithm can continue to work.
PRE : g must be a valid grid and [(p,n)] must be [(all positions visited, [all neighbours to visited Positions])]
RETURNS: The Position in n closest to p which hasn't been visited 
EXAMPLE: backtrack (mazeGenerator (createGrid (20,20)) (1,1) maze1 []) allNeigs ((mazeGenerator (createGrid (20,20)) (1,1) maze1 []) (reverse $ solver (mazeGenerator (createGrid (20,20)) (1,1) maze3 []) (1,1) [])) = (20,18)
-}
backtrack :: Grid  -> [(Position, [Position])] -> Position 
backtrack g neig = elem' pathNeigs path 
  where
    path = map fst neig
    pathNeigs = concat $ map snd neig
   
{- allNeigs g p 
Takes a list of positions and returns the tuples of that list together with it's neighbours.
PRE: p must be a valid list of Positions in g 
VARIANT: length of p 
RETURNS: [(p, [p's neighbouring Positions])]
EXAMPLE: allNeigs (mazeGenerator (createGrid (5,5)) (1,1) maze1 []) [(1,1), (2,1)]  =[((1,1),[(2,1),(1,2)]),((2,1),[(3,1),(1,1)])]
-} 

allNeigs :: Grid -> [Position] -> [(Position, [Position])]
allNeigs g [] = [] 
allNeigs g visited@((x,y):xs) = [((x,y), neig')] ++ allNeigs g xs 
  where
    neig = getNeighbours $ getCell g (x,y)
    neig' = validNeigh g (x,y) neig
{-validNeigh g p n 
picks out all neighbours of the given position that dosent have a wall between the  current position and the neighbour. 
PRE: n is valid neighbouring Position of p
VARIANT: lenght of n
RETURNS: the neighbours in n with the Wall between neighbour and p set to False.
EXAMPLE: validNeigh (array (1,2) [(1,[C (1,1) (True,False,True,True) [(2,1),(1,2)] True,C (2,1) (False,True,True,True) [(1,1),(2,2)] True]),(2,[C (1,2) (True,True,True,True) [(2,2),(1,1)] False,C (2,2) (True,True,True,True) [(1,2),(2,1)] False])]) (1,1) [(1,2),(2,1)] = [(1,2)]
-}

validNeigh :: Grid -> Position -> [Position] -> [Position]
validNeigh g p [] = []
validNeigh g (x,y) ((x',y'):ns)
  | x < x' && not (checkWalls (x,y) "e" g) = (x',y') : validNeigh g (x,y) ns
  | x > x' && not (checkWalls (x,y) "w" g) = (x',y') : validNeigh g (x,y) ns
  | y < y' && not (checkWalls (x,y) "n" g) = (x',y') : validNeigh g (x,y) ns
  | y > y' && not (checkWalls (x,y) "s" g) = (x',y') : validNeigh g (x,y) ns
  | otherwise = validNeigh g (x,y) ns
{- noBack p l 
Checks so that the new desired position hasn't been visited allready, preventing the algorithm from going back and fourth

RETURNS: True if p isn't present in l, otherwise False. 
EXAMPLE: noBack (2,2) [(1,1), (2,2)] = False
noBack (2,2) [(1,1),(2,1)] = True
-}
noBack :: Position -> [Position] -> Bool
noBack new@(x,y) old 
  | elem new old = False
  | otherwise = True 

{- elem' p1 p2 
Checks if a some position in the first list doensn't appear in the second list.

PRE: p1 and p2 cannot have exactly the same elements 
VARIANT : length of p1 
RETURNS: The Position in p1 that doesn't appear in p2
EXAMPLE: elem' [(1,1), (2,1)] [(1,1)] = (2,1) 
-}
elem' :: [Position] -> [Position] -> Position
elem' ((x,y):xs) vis
  | elem (x,y) vis == True = elem' xs vis
  | otherwise = (x,y)

---------------------------------------------------------------------------------------------------------------
                                             -- TEST CASES --
---------------------------------------------------------------------------------------------------------------
--tests
testGrid = mazeGenerator (createGrid (20,20)) (1,1) (genRandom 4) []
testList = reverse $ solver (mazeGenerator (createGrid (20,20)) (1,1) (genRandom 4) []) ((1,1), (20,20)) []
testNeig = allNeigs testGrid testList 

-- for runnign all the tests
runtestspf = runTestTT $ TestList [tpf1,tpf2,tpf3,tpf4,tpf5,tpf6, tpf7]
--test1 = TestCase $ assertEqual "vad testet gör" rätt svar (funktionen)
tpf1 = TestCase $ assertEqual "Testing noBack function" True (noBack (2,2) [(1,1), (3,3)])
tpf2 = TestCase $ assertEqual "Testing allNeigs function in a 5x5 grid" ([((1,1),[(2,1),(1,2)])]) (allNeigs (mazeGenerator (createGrid (5,5)) (1,1) (genRandom 4) []) [(1,1)]) 
tpf3 = TestCase $ assertEqual "Testing allNeigs function with in a 2x2 grid" ([((1,1),[(2,1),(1,2)])]) (allNeigs (mazeGenerator (createGrid (2,2)) (1,1) (genRandom 6) []) [(1,1)]) 
tpf4 = TestCase $ assertEqual "Testing solverfunctions endpoint, 20x20 maze" ((20,20)::(Int,Int)) (head $ reverse $ solver (mazeGenerator (createGrid (20,20)) (1,1) (genRandom 2) []) ((1,1), (20,20)) [])
tpf5 = TestCase $ assertEqual "Testing solverfunction endpoint, 10x10 maze " ((10,10)::(Int,Int)) (head $ reverse $ solver (mazeGenerator (createGrid (10,10)) (1,1) (genRandom 3) []) ((1,1), (10,10)) [] ) 
tpf6 = TestCase $ assertEqual "Testing elem'" (1,1) (elem' [(1,1), (2,1), (3,1)] [(2,1), (2,2)])
tpf7 = TestCase $ assertEqual "Testing solverfunctions endpoint, 50x50 maze" ((50,50)::(Int,Int)) (head $ reverse $ solver (mazeGenerator (createGrid (50,50)) (1,1) (genRandom 44) []) ((1,1), (50,50)) [])
