-- | Draw 2D minimap
module Draw_State2D where
import Data_structures
import Constants
import Map_Filter
import Enemy_Filter
import Utils
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
-- | Draw everything in 2D
drawAll2D:: Game_State -> Picture
drawAll2D e = Rotate (-90) $ Scale 20 20 $ Pictures[ drawMap2DAll (gameMap e)
                                                   , drawMap2D    (gameMap e)
                                                   , drawEnemies2DAll (enemies e)
                                                   , drawEnemies2D (gameMap e) (enemies e)
                                                   , drawnPlayer2D e
                                                   ]
-- | Draw the final map
drawMap2D:: GameMap -> Picture
drawMap2D  = Pictures . (map drawWall2D) . (map (paintWall blue)) . getFinalMap
-- | Draw all walls
drawMap2DAll::GameMap -> Picture
drawMap2DAll = Pictures . (map drawWall2D) . (map (paintWall black))
-- | Draw a wall
drawWall2D :: Wall -> Picture
drawWall2D w = color (wColor w) $ Line[p1W w, p2W w]
-- | Draws visible enemies with red
drawEnemies2D :: GameMap ->  Enemies -> Picture
drawEnemies2D m = Pictures . (map (drawEnemy2D red)) . (getFinalEnemies  m)
-- | Draws all enemies with green
drawEnemies2DAll :: Enemies -> Picture
drawEnemies2DAll = Pictures . (map (drawEnemy2D green))
-- | Draws an enemy with a given color
drawEnemy2D :: Color -> Enemy -> Picture
drawEnemy2D col e = color col $ Line[p1E e, p2E e]
-- | Draws the player on the minimap
drawnPlayer2D :: Game_State -> Picture
drawnPlayer2D e = Rotate (90) $ Scale 0.5 0.5 $ color red $ Polygon[(0.5,-0.5),(-0.5,-0.5),(0,0.5)]
-- | Change the color of a wall
paintWall:: Color -> Wall -> Wall
paintWall col w = w {wColor = col}

