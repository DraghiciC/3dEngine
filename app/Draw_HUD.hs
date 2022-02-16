-- | Draw HUD
module Draw_HUD where
import Data_structures
import Constants
import Draw_State2D
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
-- | Draw the health, ammo and minimap
drawHUD :: Game_State -> Picture
drawHUD e = Pictures $ [ Translate 0 (-300) $ lives (hpP (player e)) 200
                    , Translate 0 (-340) $ ammoShow (ammo (player e)) 20
                    , Translate (-300) (-300) $ drawAll2D e
                    ] ++ [crosshair] ++ [dGun]
    where
        (sxI, syI) = winSize e
        (sx, sy)   = ((fromIntegral sxI) / 2, (fromIntegral syI) / 2)
        crosshair  = target red $ min (fromIntegral(sxI)/22)(fromIntegral(syI)/22) -- | Draw crosshair
        dGun       = drawGun $ min (fromIntegral sxI)(fromIntegral syI)
-- | Draw the player's ammo
ammoShow:: Int -> Float -> Picture
ammoShow a fS | a == 0    = Blank
              | otherwise = Scale s s $ Pictures $ map (showBullet) [1..a]
    where
        s = fS / 12
        showBullet:: Int -> Picture
        showBullet p = Translate (fromIntegral(14 * (p-1))) (-10.5) bullet
-- | Draw the player's health
lives:: Float -> Float -> Picture
lives hp tS | hp * 8 / maximumHealth > 7    =Scale s s $ Pictures([f1, f2, f3, f4] ++ outlines)
            | hp * 8 / maximumHealth > 6    =Scale s s $ Pictures([f1, f2, f3, h4] ++ outlines)
            | hp * 8 / maximumHealth > 5    =Scale s s $ Pictures([f1, f2, f3]     ++ outlines)
            | hp * 8 / maximumHealth > 4    =Scale s s $ Pictures([f1, f2, h3]     ++ outlines)
            | hp * 8 / maximumHealth > 3    =Scale s s $ Pictures([f1, f2]         ++ outlines)
            | hp * 8 / maximumHealth > 2    =Scale s s $ Pictures([f1, h2]         ++ outlines)
            | hp * 8 / maximumHealth > 1    =Scale s s $ Pictures([f1]             ++ outlines)
            | otherwise                     =Scale s s $ Pictures([h1]             ++ outlines)
    where
        s    = tS/56
        f1   = Translate 0  (-5.5) $ heart
        h1   = Translate 0  (-5.5) $ halfHeart
        f2   = Translate 14 (-5.5) $ heart
        h2   = Translate 14 (-5.5) $ halfHeart
        f3   = Translate 28 (-5.5) $ heart
        h3   = Translate 28 (-5.5) $ halfHeart
        f4   = Translate 42 (-5.5) $ heart
        h4   = Translate 42 (-5.5) $ halfHeart
        out1 = Translate 0  (-5.5) $ outline
        out2 = Translate 14  (-5.5) $ outline
        out3 = Translate 28  (-5.5) $ outline
        out4 = Translate 42  (-5.5) $ outline
        outlines =[out1, out2, out3, out4]
-- | Draw crosshair
target:: Color -> Float -> Picture
target col r =  Pictures[pol, Rotate 90 pol, Rotate 180 pol, Rotate (-90) pol]
    where
        lines = [(0,0), (0,u*5), (u, u*5), (u, u), (u*5,u), (u*5, 0)]
        u = r/14
        pol = Translate (u/2) (u/2) $ Pictures[ Color col                 $ Polygon lines
                                              , Color black $ lineLoop lines]
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
