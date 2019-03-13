module Render where

import Grid
import Graphics 
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Mazegenerator
import Data.Array.IArray
import Pathfinder
import Test.HUnit
import System.Random

---------------------------------------------------------------------------------------------------------------
{-
  GameState represents the visual representation of all the different states of the game. 

  In the GameState Player (x,y) (a,b) g sp xs (t,s), x and y are Floats that defines the players positon in pixels, x' and y' are Ints and represents the players position
  in the Grid, g represents the Grid which is defined as an array containing Cells, sp is a Bool and is True if a path is shown from starting position to the exit
  and is False if there's no path shown in the Grid, xs is an infinite list of Ints that are used as seeds for generating random numbers and (t,s) contains Ints that describes 
  the max X and Y values, meaning the size of the Grid.
  
  The GameState StartMenu xs defines the start menu of the game and xs is an infinite list of Ints  that are used as seeds for generating random numbers.

  INVARIANTS: Whenever a game is started or a new g is selected, (x,y) always has an initial value of (30,30) and (a,b) an initial value of (1,1).
              (x,y) and (a,b) can never contain any non positive Floats or Ints.
              The Grid has either it's max X and Y (t,s) values set to (10,10) (20,20) or (30,30).
              Whenever a game is started or a new g is selected, the initial value of sp is set to False.
  -}

data GameState = Player (Float,Float) (Int,Int) Grid ShowPath [Int] (Int,Int)| StartMenu [Int] deriving (Show, Eq)

-- ShowPath is defined as a Bool where True means that the path is shown, and False means that the path is not shown.
type ShowPath = Bool

{-
  showAPath gs sp
  takes the current game state and returns an updated game state with the visualization of the path turned on or off
  PRE: True
  RETURNS: an updated GameState depending on sp
  EXAMPLES: showAPath (Player (30::Float,30::Float) (1,1) (array (1,1) [(1,[C (1,1) (True,True,True,True) [] False])]) False) True =
            (Player (30::Float,30::Float) (1,1) (array (1,1) [(1,[C (1,1) (True,True,True,True) [] False])]) True)
-}
showAPath :: GameState -> ShowPath -> GameState
showAPath (Player (x,y) (x',y') g sp rl end) sp' 
  | sp' == True = Player (x,y) (x',y') g True rl end
  | otherwise   = Player (x,y) (x',y') g False rl end


{-
  checkPath gs 
  takes a game state and checks if the visualization of the path is on or not
  PRE: True
  RETURNS: True if a path is shown in gs, otherwise False
  EXAMPLES: checkPath (Player (30::Float,30::Float) (1,1) (array (1,1) [(1,[C (1,1) (True,True,True,True) [] False])]) False) =
            False

-}                                         
checkPath :: GameState -> Bool
checkPath (Player (x,y) (x',y') g sp rl _) 
  | sp == True = True
  | otherwise  = False

{-
  visPath pos
  takes a position containing a pair of x and y coordinates and returns a picture of a green circle with a radius of 
  3 pixels at a fixed location.
  PRE: pos cannot contain non-positive Ints.
  RETURNS: a Picture representing a green circle at the position taken from pos
  EXAMPLES: visPath (1,2) =
            Translate (-20.0) (-20.0) (Translate 50.0 70.0 (Color (RGBA 0.0 1.0 0.0 1.0) (ThickCircle 1.5 3.0)))
-}
visPath :: Position -> Picture
visPath (x,y) = translate (-20) (-20) $ translate (fromIntegral(30+20*x)) (fromIntegral(30+20*y)) $ color green $ circleSolid 3

{- 
  visualizePath poslist
  takes a list of positions, each containing a pair of x and y coordinates, and returns a picture visualising a green circle with
  a radius of 3 on a fixed location depending on each position.
  PRE: no pair in poslist can contain negative Ints
  RETURNS: a list of Pictures where each Picture represents a green circle at the Position taken from poslist
  EXAMPLES: visualizePath [(1,2),(2,3),(4,4)] =
            [Translate (-20.0) (-20.0) (Translate 50.0 70.0 (Color (RGBA 0.0 1.0 0.0 1.0) (ThickCircle 1.5 3.0))),Translate (-20.0) (-20.0) (Translate 70.0 90.0 (Color 
            (RGBA 0.0 1.0 0.0 1.0) (ThickCircle 1.5 3.0))),Translate (-20.0) (-20.0) (Translate 110.0 110.0 (Color (RGBA 0.0 1.0 0.0 1.0) (ThickCircle 1.5 3.0)))]
-}
visualizePath :: [Position] -> [Picture]
visualizePath positions = map visPath positions 
{-
  render gs 
  renders the current state of the game and returns the picture that the updated state of the game represents
  PRE: True
  RETURNS: a Picture depending on gs
  EXAMPLES: render (Player (30::Float,30::Float) (1,1) (array (1,1) [(1,[C (1,1) (True,True,True,True) [] False])]) False) =
            Pictures [Translate 30.0 30.0 (Color (RGBA 1.0 0.0 0.0 1.0) (ThickCircle 2.5 5.0)),Pictures [Line 
            [(20.0,20.0),(20.0,40.0)],Line [(20.0,20.0),(40.0,20.0)],Line [(20.0,40.0),(40.0,40.0)],Line [(40.0,40.0),(40.0,20.0)]]]
            
            To understand the examples, call function testGame in ghci, everything printed to the display is taken care of by render.
-}
render :: GameState -> Picture
render (Player (x,y) p grid sp rl end) = scale (1.3) (1.3) $ translate size size $ pictures $ [player, maze, path]
  where
      size = realToFrac $ (-(10 * (fst end) + 10))
      player = translate (x) (y) $ color red $ circleSolid 5
      maze = pictures $ printAll $ printGrid grid n
      n = snd $ bounds grid
      path | sp == True = pictures $ visualizePath $ solver grid ((1,1),end) []
           | otherwise = blank
render (StartMenu rl) = draw 
  where
      draw  = pictures [pic1, pic2, pic3, pic4, pic5, pic6, pic7, pic8]
      pic1 = scale (1.5) (1.5) $ rotate 45 $ translate (-75) (-75) $ printPicture $ mazeGenerator (createGrid (5,5)) (1,1) (genRandom 3) []
      pic2 = translate (-350) (250) $ color red $ text "Maze Solver"
      pic3 = translate (-600) (-250) $ scale 0.2 0.2 $ color black $ text "Instructions:"
      pic4 = translate (-600) (-290) $ scale 0.2 0.2 $ color black $ text "Choose a maze by pressing 1, 2 or 3."
      pic5 = translate (-600) (-330) $ scale 0.2 0.2 $ color black $ text "Find the exit located at the far right corner." 
      pic6 = translate (-600) (-370) $ scale 0.2 0.2 $ color black $ text "Press space to show / unshow path."
      pic7 =  translate (-600) (-410) $ scale 0.2 0.2 $ color black $ text "Good Luck!" 
      pic8 = translate (300) (-410) $ scale 0.2 0.2 $ color black $ text "by: Mark, Travis & Joakim"

{-
  initialPlay1 xs
  takes a list of Ints, which are used as seeds for the random num generator, and returns a state of the game 
  consisting of a pseudo-random generated maze with the size 10x10
  PRE: True
  RETURNS: a GameState depended on what random numbers get generated with a seed from xs
  EXAMPLES: Call function testGame in ghci and check examples by pressing 1 when in the start menu
-}
initialPlay1 :: [Int] -> GameState 
initialPlay1 (r:rl) = Player (30,30) (1,1) grid False rl (10,10)
  where
    tgrid = mazeGenerator (createGrid (10,10)) (1,1) (genRandom r) []
    grid  = setWall tgrid (10,10) "e" 

{-
  initialPlay2 xs
  takes a list of Ints, which are used as seeds for the random num generator, and returns a state of the game 
  consisting of a pseudo-random generated maze with the size 20x20
  PRE: True
  RETURNS: a GameState depended on what random numbers get generated with a seed from xs
  EXAMPLES: Call function testGame in ghci and check examples by pressing 2 when in the start menu
-}
initialPlay2 :: [Int] -> GameState 
initialPlay2 (r:rl) = Player (30,30) (1,1) grid  False rl (20,20)
  where
    tgrid = mazeGenerator (createGrid (20,20)) (1,1) (genRandom r) []
    grid  = setWall tgrid (20,20) "e"

{-
  initialPlay3 xs
  takes a list of Ints, which are used as seeds for the random num generator, and returns a state of the game 
  consisting of a pseudo-random generated maze with the size 30x30
  PRE: True
  RETURNS: a GameState depended on what random numbers get generated with a seed from xs
  EXAMPLES: Call function testGame in ghci and check examples by pressing 3 when in the start menu
-}
initialPlay3 ::[Int] -> GameState 
initialPlay3 (r:rl) = Player (30,30) (1,1) grid  False rl (30,30)
  where
    tgrid = mazeGenerator (createGrid (30,30)) (1,1) (genRandom r) []
    grid  = setWall tgrid (30,30) "e"

{-
  movePlayer n gs
  takes a float which represents a direction and moves the player (+-) 20 pixels in the x or y plane.
  PRE: n cannot be a Float that takes the player outside of the grid if pos in gs is the max and min coordinates of x and y.
  RETURNS: a GameState representing the new position of the player after moving in direction represented by n
  EXAMPLES: movePlayer 1.0 (Player (30,30) (1,1) (array (1,3) [(1,[C (1,1) (True,True,True,True) [(2,1),(1,2)] False,C (2,1) (True,True,True,True) 
            [(3,1),(1,1),(2,2)] False,C (3,1) (True,True,True,True) [(2,1),(3,2)] False]),(2,[C (1,2) (True,True,True,True) [(2,2),(1,3),(1,1)] False,C (2,2) 
            (True,True,True,True) [(3,2),(1,2),(2,3),(2,1)] False,C (3,2) (True,True,True,True) [(2,2),(3,3),(3,1)] False]),(3,[C (1,3) (True,True,True,True) 
            [(2,3),(1,2)] False,C (2,3) (True,True,True,True) [(3,3),(1,3),(2,2)] False,C (3,3) (True,True,True,True) [(2,3),(3,2)] False])]) False) =

           Player (30.0,50.0) (1,2) (array (1,3) [(1,[C (1,1) (True,True,True,True) [(2,1),(1,2)] False,C (2,1) (True,True,True,True) 
           [(3,1),(1,1),(2,2)] False,C (3,1) (True,True,True,True) [(2,1),(3,2)] False]),(2,[C (1,2) (True,True,True,True) [(2,2),(1,3),(1,1)] 
           False,C (2,2) (True,True,True,True) [(3,2),(1,2),(2,3),(2,1)] False,C (3,2) (True,True,True,True) [(2,2),(3,3),(3,1)] False]),(3,[C (1,3) 
           (True,True,True,True) [(2,3),(1,2)] False,C (2,3) (True,True,True,True) [(3,3),(1,3),(2,2)] False,C (3,3) (True,True,True,True) [(2,3),(3,2)] False])] False)
 -}
movePlayer :: Float-> GameState -> GameState
movePlayer key (StartMenu rl) = StartMenu rl
movePlayer key (Player (x,y) (x',y') grid sp rl end)
  | key == (-1.0) = (Player (x,y-20.0)(x',y'-1) grid sp rl end)                            
  | (x',y') == (((fst end) + 1),(snd end)) = StartMenu rl
  | key == (1.0) = (Player (x,y+20.0) (x',y'+1) grid sp rl end)
  | key == (-2.0) = (Player (x-20.0,y) (x'-1,y') grid sp rl end)
  | key == (2.0) = (Player (x+20.0,y) (x'+1,y') grid sp rl end)
  | otherwise = (Player (x,y) (x',y') grid sp rl end)

{-
  isValidMove gs key
  takes a game state and a key, a valid input from the keyboard, and checks if that key results in a valid move or not
  PRE: key has to be a valid input from the user
  RETURNS: True if key results in a valid move depending on gs, otherwise False
  EXAMPLES: isValidMove (Player (30,30) (1,1) (array (1,1) [(1,[C (1,1) (True,True,True,True) [] False])]) False) KeyLeft =
            False
-}
isValidMove :: GameState -> SpecialKey -> Bool 
isValidMove (Player p (x,y) grid _ _ _) dir
  | KeyLeft == dir = not $ checkWalls (x,y) "w" grid
  | KeyRight== dir = not $ checkWalls (x,y) "e" grid
  | KeyUp   == dir = not $ checkWalls (x,y) "n" grid
  | KeyDown == dir = not $ checkWalls (x,y) "s" grid
                             
{-
  handleKeys key gs
  handles an input from a keyboard of the user while in the play function and changes the state of the game depending on what the key input is.
  PRE: True
  RETURNS: returns an uppdated GameState depending on key
  EXAMPLES: handleKeys (EventKey (Char 'k') Down _ _) (Player (30,30) (1,1) (array (1,1) [(1,[C (1,1) (True,True,True,True) [] False])]) False) =
            Player (30,30) (1,1) (createGrid (1,1) False) 
-}
handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (Char '1') Down _ _) (StartMenu rl) = initialPlay1 rl
handleKeys (EventKey (Char '2') Down _ _) (StartMenu rl) = initialPlay2 rl 
handleKeys (EventKey (Char '3') Down _ _) (StartMenu rl) = initialPlay3 rl
handleKeys _ (StartMenu rl)                              = StartMenu rl
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game 
  | game /= StartMenu []  && checkPath game == True  = showAPath game False   
  | game /= StartMenu []  && checkPath game == False = showAPath game True                                         
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) game@(Player _ _ _ _ rl _) 
  | game /= StartMenu []       = StartMenu rl
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game    
  | isValidMove game KeyUp    = movePlayer 1.0 game
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game  
  | isValidMove game KeyDown  = movePlayer (-1.0) game
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game  
  | isValidMove game KeyLeft  = movePlayer (-2.0) game
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game 
  | isValidMove game KeyRight = movePlayer 2.0 game
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) game@(Player _ _ _ _ rl _)
  | game /= StartMenu []        = StartMenu rl
handleKeys _ game = game

--frames per second set to 60
fps :: Int
fps = 60

-- the infinite list used as seeds for the random number generator.
mazelist :: [Int]
mazelist = [1..]

---------------------------------------------------------------------------------------------------------------
                                             -- TEST CASES --
---------------------------------------------------------------------------------------------------------------
-- All functions that returns a picture, an updated GameState and the function handleKeys are fully tested using calling testGame in ghci. This is to be 
-- absolutely sure that they are functioning the way we want them to. Testing is also easier this way as it visually shows what they 
-- are supposed to do.

-- a test of the full game
testGame = play (FullScreen) white fps (StartMenu mazelist) render handleKeys movePlayer

rt1 = TestCase $ assertEqual "checking isValidMove on a move that takes the player outside the grid" False (isValidMove (initialPlay1 mazelist) KeyDown)
rt2 = TestCase $ assertEqual "checking isValidMove on a move that takes the player outside the grid" False (isValidMove (initialPlay1 mazelist) KeyLeft)
rt3 = TestCase $ assertEqual "checking isValidMove on a move that moves the player inside the grid" True (isValidMove (initialPlay1 mazelist) KeyUp)
rt4 = TestCase $ assertEqual "checking movePlayer with an invalid Float input, should return same GameState" (initialPlay1 mazelist) (movePlayer 3.0 (initialPlay1 mazelist))
rt5 = TestCase $ assertBool "checking movePlayer with a valid Float input, should return a different GameState" ((initialPlay1 mazelist) /= (movePlayer 2.0 (initialPlay1 mazelist)))
rt6 = TestCase $ assertEqual "checking visualizePath to see that length of output corresponds to lenght of input" 3 (length $ visualizePath [(1,1),(2,2),(3,3)])
rt7 = TestCase $ assertEqual "checking checkPath on all initial GameStates" [False,False,False] (map checkPath [(initialPlay1 mazelist), (initialPlay2 mazelist), (initialPlay3 mazelist)])
rt8 = TestCase $ assertBool "checking checkPath on two different GameStates, one with Path set to True and one with it set to False" $ (checkPath (initialPlay1 mazelist)) /= (checkPath (Player (30,30) (1,1) (mazeGenerator (createGrid (20,20)) (1,1) (genRandom 5) []) True mazelist (10,10)))

-- runs all the test cases above.
runtestsr = runTestTT $ TestList [rt1,rt2,rt3,rt4,rt5,rt6,rt7,rt8]

