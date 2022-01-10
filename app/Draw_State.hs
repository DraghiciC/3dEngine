-- | Draw the gamestate
module Draw_State where
import Data_structures
import Draw_State3D
import Draw_HUD
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
-- | Draw states
drawState :: Game_State -> Picture
drawState e | (menu e) == MenuPlay = drawStatePlay e -- | playing the game
            | (menu e) == MenuGameOver = Translate 0 0 $ scale 1 1 $ text $ "Game Over" -- | losing all health
            | otherwise = Translate 0 0 $ scale 1 1 $ text $ "You win!" -- | finishing the game
-- | Draw the current state
drawStatePlay :: Game_State -> Picture
drawStatePlay e = Pictures[ drawAll3D e
                          , drawHUD e
                          ]

