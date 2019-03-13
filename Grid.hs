module Grid (Cell (Empty), Walls, Grid, createGrid, getCellInfo, getPosition, getWalls, getNeighbours, checkWalls, setWall, getCell, getVisited, setVisited)  where
import Test.HUnit
import Data.Array.IArray

-- Position represents the (x,y) coordinates of a Cell
type Position = (Int,Int)
-- Size represents the size of a Grid, example (20,20) means that the grid is made up with 20 lists with 20 cells each.
type Size = (Int, Int)

-- Walls represents the four walls around a Cell and hold information if they exists or not. False/True = No Wall/Wall. (East,West,North,South)
type Walls = (Bool,Bool,Bool,Bool)

{- A Cell represent a tile on the grid. Each Cell contains a position, a list of neighbours and status of the walls around the cell. The Cell also keeps track if it has been visited or not by the pathfinder.

 In the Cell C (x,y) (w,e,n,s) [(x',y')..] visited, (x,y) is the position of the cell in a grid, where y gives which list in an Array the Cell is located, and x gives the position the Cell is located in the list. (w,e,n,s) are booleans that shows if a wall is open or closed, True for closed and False for open. [(x',y')..] is a list of the cells neighbour positions. visited is a boolean that represents if the cell has been modified by the maze generator, True if modified, otherwise False.

 INVARIANT: C Position Walls neighbours visited
            Position is non negative numbers.
            Visited is always False when the Cell is created.
            All walls is True when the Cell is created.
            Neighbours stores valid positions of the Cells closest to the current Cell. 
-}
data Cell = Empty 
           | C Position Walls [Position] Bool deriving (Show,Eq)

{- A Grid represent a two dimensional gameboard which the maze is created on. Each list of cells in the Grid has a key mapped to it.
   The Cell with Position (x,y) is the element at position x in the list that is mapped to the key y.
-}
type Grid = Array Int [Cell]

{- createGrid (x,y)
   creates a Grid with a cell in every position in the grid,from position (1,1) up to the provided size (x,y). The Grid will map all values between 1-y to lists containing x amount of cells inside.

   PRE:     x and y has the same value and is non negative.
   RETURNS: an Array with y lists inside. each of those list contains x Cells.
   EXAMPLE: createGrid (2,2) = array (1,2) [(1,[C (1,1) (True,True,True,True) [(2,1),(1,2)] False,C (2,1) (True,True,True,True) [(1,1),(2,2)] False]),(2,[C (1,2) (True,True,True,True) [(2,2),(1,1)] False,C (2,2) (True,True,True,True) [(1,2),(2,1)] False])]
-}
createGrid :: Size -> Grid
createGrid (x,y) = createGrid' (x,y) grid
  where
    grid = listArray (1,y) (replicate y []) :: Grid

{- createGrid' (x,y) grid
   creates an Grid with cells in every position in the grid, starting from position (1,1) up to the provided size.

   PRE:     grid has the index bounds 1-y.
   RETURNS: an Array with y lists inside. each of those list contains x Cells
   VARIANT: y
   EXAMPLE: createGrid (2,2) = array (1,2) [(1,[C (1,1) (True,True,True,True) [(2,1),(1,2)] False,C (2,1) (True,True,True,True) [(1,1),(2,2)] False]),(2,[C (1,2) (True,True,True,True) [(2,2),(1,1)] False,C (2,2) (True,True,True,True) [(1,2),(2,1)] False])]
-}
createGrid' :: Size -> Grid -> Grid
createGrid' (x,y) grid
  | y == 0 = grid
  | otherwise = createGrid' (x,(y-1)) grid'
  where
    grid' = grid // [(y,(reverse $ createRow (x,y) x))]

{- createRow (x,y) max
   Creates a row with cells.

   PRE: x is non neagative. max is the highest y value of the Grid where the list is going to be inserted.
   RETURNS: list containing x Cells
   VARIANT: x
   EXAMPLE: createRow (2,2) 2 = [C (2,2) (True,True,True,True) [(1,2),(2,1)] False,C (1,2) (True,True,True,True) [(2,2),(1,1)] False]
-}
createRow :: Position -> Int -> [Cell]
createRow (x,y) max
  | x == 0 = []
  | otherwise =  (createCell (x,y)) max : createRow ((x-1), y) max

{- createCell (x,y) max
   creates a Cell with the provided position. max is used to secure that the cells neighbours do not appear outside the grid.

   PRE: x and y is valid cordinates in a grid. max is the size of the same grid.
   RETURNS: A Cell with the position (x,y).
   EXAMPLE: createCell (2,2) 2 = C (2,2) (True,True,True,True) [(1,2),(2,1)] False
-}
createCell :: Position -> Int -> Cell
createCell p max = C p (True,True,True,True) (addNeighbours p max) False

{- addNeighbours (x,y) max
   Calculates the cordinates to all neighbours of the provided position.

   PRE: (x,y) is non negative. max is the size of the grid about to be created.
   RETURNS: A List with cordinates to neighbours of (x,y)
   EXAMPLE: addNeighbours (2,2) 2 = [(1,2),(2,1)]
-}
addNeighbours :: Position -> Int -> [Position]
addNeighbours _ 1 = []
addNeighbours (x,y) max = checkX ++ checkY
   where
     n = (x,(y+1))
     s = (x,(y-1))
     w = ((x-1),y)
     e = ((x+1),y)
     checkX
       | x > 1 && x < max = [e,w]
       | x > 1            = [w]
       | otherwise        = [e]
     checkY
       | y > 1 && y < max = [n,s]
       | y > 1            = [s]
       | otherwise        = [n]

-- Returns the position of a Cell
getPosition :: Cell -> Position
getPosition (C p _ _ _) = p

-- Returns the walls of a Cell
getWalls :: Cell -> Walls
getWalls (C _ w _ _) = w

{- setWall grid (x,y) s
   opens the wall between the position (x,y) and one of its neighbours. Whitch neighbour is given by the provided direction. "w", "e", "n" and south stands for the west, east, north and south wall. 

   PRE: (x,y) is a valid position in grid. s is "w","e","n" or "s".
   RETURNS: a copy of grid, but with a wall of the cell with position (x,y) sat to False. which wall depends on s.
   EXAMPLE: setWall array (1,2) [(1,[C (1,1) (True,True,True,True) [(2,1),(1,2)] False,C (2,1) (True,True,True,True) [(1,1),(2,2)] False]),(2,[C (1,2) (True,True,True,True) [(2,2),(1,1)] False,C (2,2) (True,True,True,True) [(1,2),(2,1)] False])] (1,1) "e" = array (1,2) [(1,[C (1,1) (True,True,True,True) [(2,1),(1,2)] False,C (2,1) (True,True,True,True) [(1,1),(2,2)] False]),(2,[C (1,2) (True,True,True,True) [(2,2),(1,1)] False,C (2,2) (True,True,True,True) [(1,2),(2,1)] False])]
-}
setWall :: Grid -> Position -> String -> Grid
setWall grid p [] = grid 
setWall grid (x,y) s = grid'
  where
    grid' = grid // [(y, (setWall' (grid ! y) (x,y) s))]
  
{- setWall' row (x,y) s
   opens the wall between the position (x,y) and one of its neighbours. Whitch neighbour is given by the provided direction. "w", "e", "n" and "s" stands for the west, east, north and south wall. 

   PRE: (x,y) is a valid position in grid. s is "w","e","n" or "s".
   RETURNS: a copy of row, but with a wall of the cell with position (x,y) changed to False. which wall depends on s.
   VARIANT: length of row
   EXAMPLE: setWall' [C (1,1) (True,True,True,True) [] False] (1,1) "e" = [C (1,1) (True,False,True,True) [] False]
            setWall' [C (1,1) (True,True,True,True) [] False] (1,1) "n" = [C (1,1) (True,True,False,True) [] False]
-}
setWall' :: [Cell] -> Position -> String -> [Cell]
setWall' [] p key = []
setWall' ((C p' (w,e,n,s) neigh v:xs)) p key
    | p' == p && key == "w" = (C p' (False,e,n,s) neigh) v : setWall' xs p key
    | p' == p && key == "e" = (C p' (w,False,n,s) neigh) v : setWall' xs p key
    | p' == p && key == "n" = (C p' (w,e,False,s) neigh) v : setWall' xs p key
    | p' == p && key == "s" = (C p' (w,e,n,False) neigh) v : setWall' xs p key
    | otherwise = (C p' (w,e,n,s) neigh v) : (setWall' xs p key)

-- returns the status of if the cell is visited or not.
getVisited :: Cell -> Bool
getVisited (C _ _ _ v) = v

{- setVisited grid (x,y) vis
   change the visited status of a cell in a grid.

   PRE: (x,y) is a valid position in grid.
   RETURNS: A copy of grid, but with the visited staus cahnged to vis in the Cell with position (x,y).
   EXAMPLE: setVisited (array (1,1) [(1,[C (1,1) (True,True,True,True) [] False])]) (1,1) True = array (1,1) [(1,[C (1,1) (True,True,True,True) [] True])]
 
-}
setVisited :: Grid -> Position -> Bool -> Grid
setVisited grid (x,y) vis = grid'
  where
    grid' = grid // [(y,(setVisited' (grid ! y) (x,y) vis))]

{- setVisited' row (x,y) vis
   change the visited status of a cell in a row.

   PRE: (x,y) is a valid position in row.
   RETURNS: A copy of row, but with the visited staus cahnged to vis in the Cell with position x.
   VARIANT: length of row
   EXAMPLE: setVisited' [(1,[C (1,1) (True,True,True,True) [] False])] (1,1) True = array (1,1) [(1,[C (1,1) (True,True,True,True) [] True])]
-}
setVisited' :: [Cell] -> Position -> Bool -> [Cell]
setVisited' [] _ _ = []
setVisited' ((C p' w n v' :xs)) p v
  | p' == p = (C p' w n v) : setVisited' xs p v
  | otherwise = (C p' w n v') : setVisited' xs p v

-- returns the neighbours of a cell.
getNeighbours :: Cell -> [Position]
getNeighbours (C _ _ n _) = n

-- Returns the Position, Walls and Neighbours of a Cell in a Tuple.
getCellInfo :: Cell -> (Position, Walls, [Position], Bool)
getCellInfo Empty = ((0,0), (False,False,False,False), [(0,0)], False)
getCellInfo (C p w n v) = (p, w, n, v)

{- getCell grid (x,y)
   finds and returns the Cell matcing the provided position.

   PRE: (x,y) must be a valid position in grid
   RETURNS: The Cell assosiated to (x,y) located in grid
   EXAMPLE: getCell (array (1,2) [(1,[C (1,1) (True,True,True,True) [(2,1),(1,2)] False,C (2,1) (True,True,True,True) [(1,1),(2,2)] False]),(2,[C (1,2) (True,True,True,True) [(2,2),(1,1)] False,C (2,2) (True,True,True,True) [(1,2),(2,1)] False])]) (1,1) = C (1,1) (True,True,True,True) [(2,1),(1,2)] False
-}
getCell :: Grid -> Position -> Cell
getCell grid (x,y) = getCell' (grid ! y) (x,y)

{- getCell' xs (x,y)
   finds the Cell matcing the provided position.

   PRE: (x,y) must be a valid position in xs
   RETURNS: Cell assosiated to (x,y) located in xs
   VARIANT: length xs
   EXAMPLE: getCell (array (1,2) [(1,[C (1,1) (True,True,True,True) [(2,1),(1,2)] False,C (2,1) (True,True,True,True) [(1,1),(2,2)] False]),(2,[C (1,2) (True,True,True,True) [(2,2),(1,1)] False,C (2,2) (True,True,True,True) [(1,2),(2,1)] False])]) (1,1) = C (1,1) (True,True,True,True) [(2,1),(1,2)] False
-}
getCell' :: [Cell] -> Position -> Cell
getCell' [] p = Empty
getCell' (x:xs) p
  | p == p' = x
  | otherwise  = getCell' xs p
  where
    (C p' _ _ _) = x

{- checkWalls (x,y) s grid 
   check if there is a wall in the direction the player tries to go.

   PRE: (x,y) is a valid position in grid. 
   RETURNS: True if there is a wall in the direction s in c with cordinates (x,y), otherwise False.
   EXAMPLE: checkWalls (x,y) "w" [[C (x,y) (True,False,False,False)]] = True
            checkWalls (x,y) "e" [[C (x,y) (True,False,False,False)]] = False
-}
checkWalls :: Position -> String -> Grid -> Bool
checkWalls (x,y) s c
  | s == "w" = west
  | s == "e" = east
  | s == "n" = north
  | s == "s" = south
  where
    (C _ walls _ _) = getCell c (x,y)
    (west, east, north, south) = walls

---------------------------------------------------------------------------------------------------------------
                                             -- TEST CASES --
---------------------------------------------------------------------------------------------------------------

-- Tests for addNeighbours ------------------------------------------------------

testAddNeighbours = runTestTT $ TestList [testNeighbour1, testNeighbour2]
testNeighbour1 = TestCase $ assertEqual "Testing 1x1 grid" [] (addNeighbours (1,1) 1)
testNeighbour2 = TestCase $ assertEqual "Testing 5x5 grid, position (2,2)"  [(3,2),(1,2),(2,3),(2,1)] (addNeighbours (2,2) 5)

-- Tests for checkWalls ---------------------------------------------------------

testCheckWalls = TestCase $ assertEqual "Testing checkwalls west" True (checkWalls (1,1) "e" (array (1,2) [(1,[C (1,1) (True,True,True,True) [(2,1),(1,2)] False,C (2,1) (True,True,True,True) [(1,1),(2,2)] False]),(2,[C (1,2) (True,True,True,True) [(2,2),(1,1)] False,C (2,2) (True,True,True,True) [(1,2),(2,1)] False])]))

-- Tests for setWall ------------------------------------------------------------

testsetWalls = runTestTT $ TestList [testWestWall, testEastWall, testNorthWall, testSouthWall]

testWestWall = TestCase $ assertEqual "Testing west" (array (1,1) [(1,[C (1,1) (False,True,True,True) [] False])]) (setWall (createGrid (1,1)) (1,1) "w")
testEastWall = TestCase $ assertEqual "Testing east" (array (1,1) [(1,[C (1,1) (True,False,True,True) [] False])]) (setWall (createGrid (1,1)) (1,1) "e") 
testNorthWall = TestCase $ assertEqual "Testing north" (array (1,1) [(1,[C (1,1) (True,True,False,True) [] False])]) (setWall (createGrid (1,1)) (1,1) "n") 
testSouthWall = TestCase $ assertEqual "Testing south" (array (1,1) [(1,[C (1,1) (True,True,True,False) [] False])]) (setWall (createGrid (1,1)) (1,1) "s") 


-- Tests for getCellInfo --------------------------------------------------------

testGetCellInfo = TestCase $ assertEqual "Testing function getCellInfo" ((1,1),(True,True,True,True),[(2,1),(1,2)],False) (getCellInfo $ getCell (createGrid (5,5)) (1,1))

-- Tests for createGrid ---------------------------------------------------------

testCreateGrid = runTestTT $ TestList [testGrid1, testGrid2]

testGrid1 = TestCase $ assertEqual "Testing 1x1 grid" (array (1,1) [(1,[C (1,1) (True,True,True,True) [] False])]) (createGrid (1,1))
testGrid2 = TestCase $ assertEqual "Testing 3x3 grid" (array (1,3) [(1,[C (1,1) (True,True,True,True) [(2,1),(1,2)] False,C (2,1) (True,True,True,True) [(3,1),(1,1),(2,2)] False,C (3,1) (True,True,True,True) [(2,1),(3,2)] False]),(2,[C (1,2) (True,True,True,True) [(2,2),(1,3),(1,1)] False,C (2,2) (True,True,True,True) [(3,2),(1,2),(2,3),(2,1)] False,C (3,2) (True,True,True,True) [(2,2),(3,3),(3,1)] False]),(3,[C (1,3) (True,True,True,True) [(2,3),(1,2)] False,C (2,3) (True,True,True,True) [(3,3),(1,3),(2,2)] False,C (3,3) (True,True,True,True) [(2,3),(3,2)] False])]) (createGrid (3,3))



-- Runs all tests in the file Grid.hs
testGridFile = runTestTT $ TestList [testNeighbour1, testNeighbour2, testWestWall, testEastWall, testNorthWall, testSouthWall, testCheckWalls, testGetCellInfo, testGrid1, testGrid2]
