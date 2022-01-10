-- | Draw 3d
module Draw_State3D where
import Data_structures
import Constants
import Map_Filter
import Enemy_Filter
import Utils
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Data.List

-- | Draw the entire state in 3D
drawAll3D::Game_State -> Picture
-- |drawAll3D e = Pictures $ [floor, sky] ++ (test (aux e) (drawParts e m en)) ++ [crosshair] ++ [dGun]
drawAll3D e = Pictures $ [floor, sky] ++ drawParts e m en ++ [crosshair] ++ [dGun]
    where
        (sxI, syI) = winSize e
        (sx, sy)   = ((fromIntegral sxI) / 2, (fromIntegral syI) / 2)
        en         = getFinalEnemies (gameMap e) (enemies e)
        m          = getFinalMap (gameMap e)
        floor      = Color (makeColor (138/255) (69/255)  (19/255)  1) $ Polygon [(-sx, 0), (sx,0), (sx, -sy), (-sx, -sy)] -- | Draw the floor
        sky        = Color (makeColor (135/255) (206/255) (250/255) 1) $ Polygon [(-sx, 0), (sx,0), (sx,  sy), (-sx, sy)] -- | Draw the sky
        crosshair  = target red $ min (fromIntegral(sxI)/22)(fromIntegral(syI)/22) -- | Draw crosshair
        dGun       = drawGun $ min (fromIntegral sxI)(fromIntegral syI)
-- | Test for ilustrations
test:: Int -> [Picture] -> [Picture]
test t p | t /= 0 = (head p) : (test (t-1) (tail p))
         | otherwise = []
-- | Draw all level parts
drawParts:: Game_State -> GameMap -> Enemies -> [Picture]
drawParts e [] [] = []
drawParts e w  [] = map (drawWall3D e)  w
drawParts e [] en = map (drawEnemy3D e) en
drawParts e m  en | (null ven) && (null vw) && (dw < den) = ((drawEnemy3D e  (head en)) : (drawParts e  m       (tail en)))
                  | (null ven) && (null vw)               = ((drawWall3D  e  (head m )) : (drawParts e (tail m)  en      ))
                  | otherwise                             = (drawParts    e   hw hen) ++ (map (drawWall3D e) vw)++ (map (drawEnemy3D e) ven) 
    where
        dw  = distWall  (head m)
        den = distEnemy (head en)
        (ven, hen) = partition (isEnemyFullyVisible m en) en
        (vw , hw ) = partition (isWallFullyVisible  m en) m
-- | Draw a wall
drawWall3D:: Game_State -> Wall -> Picture
drawWall3D e w = drawShape3D e (wh w) (wColor w) (p1W w) (p2W w) 0
-- | Draw an enemy
drawEnemy3D:: Game_State -> Enemy -> Picture
drawEnemy3D e en = drawShape3D e (eh en) c (p1E en) (p2E en) 1
    where
        c = makeColor (217/255) (39/255) (56/255) 1
-- | Draw a shape
drawShape3D:: Game_State -> Float -> Color -> Coord -> Coord -> Int -> Picture
drawShape3D e h col p1 p2 0 = Pictures[ Color col                 $ Polygon  (getCornerPoints e h p1 p2) -- | Body
                                      , Color (contrastColor col) $ lineLoop (getCornerPoints e h p1 p2) -- | Outline
                                      ]
drawShape3D e h col p1 p2 1 = Pictures[ Color col                 $ Polygon  (getCornerPoints e h p1 p2) -- | Body
                                      , Color (contrastColor col) $ lineLoop (getCornerPoints e h p1 p2) -- | Outline
                                      , Color black               $ line     [(x2-(x2-xc)/2,y2/2), (xc+(x2-xc)/5, 0)] -- | Enemy face
                                      , Color black               $ line     [(x3-(x3-xc)/2,y3/2), (xc+(x3-xc)/5, 0)]
                                      , Color black               $ lineLoop     [(x1-(x1-xc)/2,y1/3), (x1-(x1-xc)/4,y1/2), (x4-(x4-xc)/4,y4/2), (x4-(x4-xc)/2,y4/3)]
                                      ]
    where
        [(x1,y1), (x2,y2), (x3,y3), (x4,y4)] = getCornerPoints e h p1 p2
        xc  = (x1+x3)/2
        -- | ncol = mixColors 0.5 0.5 black col
-- | Translate coordinates from 2d map to coodrinates on the screen
getCornerPoints::Game_State -> Float -> Coord -> Coord -> [Coord]
getCornerPoints e h (x1, y1) (x2, y2) = [(xW1,pH1), (xW1,h1), (xW2,h2), (xW2,pH2)] 
    where
        d1  = distCoord (0,0) (x1, y1)
        d2  = distCoord (0,0) (x2, y2)
        hW  = h - playerHeigth
        ratio = nearPlane * (realToFrac(fst $ winSize e) / ((snd $ head viewBox)*2))
        pH1 =  (playerHeigth/d1) * ratio 
        pH2 =  (playerHeigth/d2) * ratio
        h1  = -(hW / d1)         * ratio
        h2  = -(hW / d2)         * ratio
        xW1 =  (y1 / x1)         * ratio
        xW2 =  (y2 / x2)         * ratio
-- | Draw crosshair
target:: Color -> Float -> Picture
target col r =  Pictures[pol, Rotate 90 pol, Rotate 180 pol, Rotate (-90) pol]
    where
        lines = [(0,0), (0,u*5), (u, u*5), (u, u), (u*5,u), (u*5, 0)]
        u = r/14
        pol = Translate (u/2) (u/2) $ Pictures[ Color col                 $ Polygon lines
                                              , Color (contrastColor col) $ lineLoop lines]
-- | Draw gun
drawGun:: Float -> Picture
drawGun r =  Pictures[pol]
    where
        lines0 = [(0, 0), (0, u), (u*3, 0), (u*3, u*(-2)), (0,0)]
        lines1 = [(u*3, 0), (u*3, u*(-6)), (u*2, u*(-6)), (u*2, u*(-1.33))]
        lines2 = [(u*3, 0), (u*3, u*(-6)), (u*5, u*(-6)), (u*5, 0), (u*3, 0)]
        lines3 = [(0, u), (u, u), (u*5, 0), (u*3, 0), (0, u)]
        u = r/12
        pol = Translate (u*3) (u*(-3)) $ Pictures[     Color red   $ Polygon lines0
                                              , Color black $ lineLoop lines0
                                              , Color (makeColor (200/255) (0/255) (0/255) 1)   $ Polygon lines1
                                              , Color black $ lineLoop lines1
                                              , Color red   $ Polygon lines2
                                              , Color black $ lineLoop lines2
                                              , Color (makeColor (200/255) (50/255) (50/255) 1)   $ Polygon lines3
                                              , Color black $ lineLoop lines3
                                              ]