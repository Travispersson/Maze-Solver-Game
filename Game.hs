module Game where 
import Render
import Graphics.Gloss.Interface.Pure.Game

{-
  main 
  Runs the game in full screen mode.
  SIDE EFFECTS: printing Pictures to the display of the user and registers inputs from the users keyboard.
 -}
main = do 
    play (FullScreen) white fps (StartMenu mazelist) render handleKeys movePlayer