module Graphics where
import Grid
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Data.Array.IArray
import Test.HUnit

{-printCell c
Takes a cell and creates the coordinates for each line that is needed to display the cell in Gloss
RETURNS : [[(Startpoint, Endpoint)]] for each wall in c
EXAMPLE: printCell C (2,2) (True,True,True,True) [(3,2),(1,2),(2,3),(2,1)] False = [[(40.0,40.0),(40.0,60.0)],[(40.0,40.0),(60.0,40.0)],[(40.0,60.0),(60.0,60.0)],[(60.0,60.0),(60.0,40.0)]]
printCell C (1,1) (True,True,True,True) [(2,1),(1,2)] False = [[(20.0,20.0),(20.0,40.0)],[(20.0,20.0),(40.0,20.0)],[(20.0,40.0),(40.0,40.0)],[(40.0,40.0),(40.0,20.0)]]
-}

printCell :: Cell -> [[(Float, Float)]]
printCell c = [l1, l2, l3, l4]
  where
    pos = getPosition c
    (w,e,n,s) = getWalls c
    startx = 20 * (realToFrac $ fst pos)
    starty = 20 * (realToFrac $ snd pos)
    l1 = checkWall w [(startx, starty), (startx, (starty + 20.0))]
    l2 = checkWall s ([(startx, starty), ((startx + 20.0) , (starty))])
    l3 = checkWall n ([(startx, (starty + 20.0)), ((startx + 20.0), (starty + 20.0))])
    l4 = checkWall e ([((startx + 20), (starty + 20)), ((startx + 20), (starty))])

{-Checkwall b l
Checks if a wall will be built, depending on the Grid-layout
RETURNS: [(Startpoint, Endpoint)] for l if b==True, otherwise nothing
EXAMPLES: checkWall False [(20.0,20.0),(20.0,40.0)] = []
checkWall True [(20.0,20.0),(20.0,40.0)] = [(20.0,20.0),(20.0,40.0)]
-}
checkWall :: Bool -> [(Float, Float)] -> [(Float, Float)]
checkWall w l
  | w == True = l
  | otherwise = []

{-printRow c
Concatenates the start and endpoints for all lines in a row
RETURNS: [[(Startpoint, Endpoint)]] for all lines in c
VARIANT: lenght of c
EXAMPLES: printGrid' [C (1,2) (True,True,True,True) [(2,2),(1,3),(1,1)] False,C (2,2) (True,True,True,True) [(3,2),(1,2),(2,3),(2,1)] False,C (3,2) (True,True,True,True) [(2,2),(3,3),(3,1)] False] = [[(20.0,40.0),(20.0,60.0)],[(20.0,40.0),(40.0,40.0)],[(20.0,60.0),(40.0,60.0)],[(40.0,60.0),(40.0,40.0)],[(40.0,40.0),(40.0,60.0)],[(40.0,40.0),(60.0,40.0)],[(40.0,60.0),(60.0,60.0)],[(60.0,60.0),(60.0,40.0)],[(60.0,40.0),(60.0,60.0)],[(60.0,40.0),(80.0,40.0)],[(60.0,60.0),(80.0,60.0)],[(80.0,60.0),(80.0,40.0)]]
-}
printRow :: [Cell] -> [[(Float, Float)]]
printRow [x] = printCell x
printRow (x:xs) =  printCell x ++ printRow xs

{- printGrid g i
Concatenates the start and endpoints for all lines in a grid
RETURNS: [[(Startpoint, Endpoint)]] for all rows up to and including i
VARIANT: length of g
EXAMPLES: printGrid (createGrid (1,1) 1) = [[(20.0,20.0),(20.0,40.0)],[(20.0,20.0),(40.0,20.0)],[(20.0,40.0),(40.0,40.0)],[(40.0,40.0),(40.0,20.0)]]
 -}
printGrid :: Grid -> Int -> [[(Float, Float)]]
printGrid grid 1 = printRow (grid ! 1)
printGrid grid n = printRow (grid ! n) ++ printGrid grid (n-1) 


{-printAll l
Creates a list of pictures for all lines 
RETURNS: A list of Pictures from all coordinates in l
VARIANT: length of l
EXAMPLES : printAll [[(20.0,20.0),(20.0,40.0)],[(20.0,20.0),(40.0,20.0)],[(20.0,40.0),(40.0,40.0)],[(40.0,40.0),(40.0,20.0)]] = [Line [(20.0,20.0),(20.0,40.0)],Line [(20.0,20.0),(40.0,20.0)],Line [(20.0,40.0),(40.0,40.0)],Line [(40.0,40.0),(40.0,20.0)]]
-}
printAll :: [[(Float, Float)]] -> [Picture]
printAll [x] = [line x]
printAll (x:xs) = [line x] ++ printAll xs

{-printPicture g
Creates a single picture from a grid of cells.
RETURNS: A picture of all cell walls in g
EXAMPLES : printPicture array (1,1) [(1,[C (1,1) (True,True,True,True) [] False])] = Pictures [Line [(20.0,20.0),(20.0,40.0)],Line [(20.0,20.0),(40.0,20.0)],Line [(20.0,40.0),(40.0,40.0)],Line [(40.0,40.0),(40.0,20.0)]]
-}
printPicture :: Grid -> Picture
printPicture grid = pictures ((printAll $ printGrid grid n))
  where n = snd $ bounds grid


---------------------------------------------------------------------------------------------------------------
                                             -- TEST CASES --
---------------------------------------------------------------------------------------------------------------
-- We have tested all of the functions that produce a Picture inside of Gloss. This is to be absolutely sure that they are functioning 
-- the way we want them to. Testing is also easier this way as it visually shows what they are supposed to do.
runtestsG = runTestTT $ TestList [tG1, tG2, tG3, tG4, tG5]
tG1 = TestCase $ assertEqual "testing printCell" [[(20.0,20.0),(20.0,40.0)],[(20.0,20.0),(40.0,20.0)],[(20.0,40.0),(40.0,40.0)],[(40.0,40.0),(40.0,20.0)]] (printCell $ getCell (createGrid (3,3)) (1,1))
tG2 = TestCase $ assertEqual "testing checkWall, no wall" [] (checkWall False [(20.0,20.0),(20.0,40.0)])
tG3 = TestCase $ assertEqual "testing checkWall with wall" [(20.0,20.0),(20.0,40.0)] (checkWall True [(20.0,20.0),(20.0,40.0)]) 
tG4 = TestCase $ assertEqual "testing printRow" [[(20.0,40.0),(20.0,60.0)],[(20.0,40.0),(40.0,40.0)],[(20.0,60.0),(40.0,60.0)],[(40.0,60.0),(40.0,40.0)],[(40.0,40.0),(40.0,60.0)],[(40.0,40.0),(60.0,40.0)],[(40.0,60.0),(60.0,60.0)],[(60.0,60.0),(60.0,40.0)],[(60.0,40.0),(60.0,60.0)],[(60.0,40.0),(80.0,40.0)],[(60.0,60.0),(80.0,60.0)],[(80.0,60.0),(80.0,40.0)]] (printRow $ (createGrid (3,3) ! 2 ))
tG5 = TestCase $ assertEqual "testing printPicture" (Pictures [Line [(20.0,20.0),(20.0,40.0)],Line [(20.0,20.0),(40.0,20.0)],Line [(20.0,40.0),(40.0,40.0)],Line [(40.0,40.0),(40.0,20.0)]]) (printPicture $ createGrid (1,1))
