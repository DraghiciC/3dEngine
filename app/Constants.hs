-- | Constants
module Constants where
import Data_structures
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
-- | Player's health
maximumHealth :: Float
maximumHealth = 8
-- | Rotation speed
arrowRotationSpeed :: Float
arrowRotationSpeed = 100
-- | Player's walk speed
walkSpeed :: Float
walkSpeed = 5
-- | Player's run speed
runSpeed :: Float
runSpeed = 10
-- | Player's height
playerHeigth::Float
playerHeigth = 0.5
-- | FOV
viewAngle :: Float
viewAngle = 90
-- | Near plane distance
nearPlane :: Float
nearPlane = 0.001
-- | Far plane distance
farPlane :: Float
farPlane = 50
-- | Wall visible precision
precisionWallHidden :: Float
precisionWallHidden = 100
-- | Wall distance precision
precisionWallDist :: Float
precisionWallDist = 100
-- | Enemy visible precision
precisionEnemyHidden :: Float
precisionEnemyHidden = 200
-- | Enemy distance precision
precisionEnemyDist :: Float
precisionEnemyDist = 100

-- | HP full sprite
heart :: Picture
heart = Pictures [r1, r2, r3, r4, r5]
    where
        pixel = Polygon[(0,0), (0,1), (1,1), (1,0)]
        bR1 = Color (makeColor 1     0.855 0.753 1) pixel
        bR2 = Color (makeColor 0.878 0     0     1) pixel
        bR3 = Color (makeColor 0.996 0     0     1) pixel
        bR4 = Color (makeColor 0.992 0.153 0.153 1) pixel
        bR5 = Color (makeColor 0.996 0.29  0.294 1) pixel
        r1  = Pictures [ Translate 2  9 bR1, Translate 3  9 bR1
                       , Translate 4  8 bR1, Translate 5  7 bR1
                       , Translate 6  7 bR1, Translate 9  9 bR1
                       , Translate 10 8 bR1
                       ]
        r2  = Pictures [ Translate 6  1 bR2, Translate 5  2 bR2
                       , Translate 4  3 bR2, Translate 3  4 bR2
                       , Translate 2  5 bR2, Translate 1  6 bR2
                       , Translate 1  7 bR2
                       ]
        r3  = Pictures [ Translate 1  8 bR3, Translate 2  7 bR3
                       , Translate 3  6 bR3, Translate 4  5 bR3
                       , Translate 5  4 bR3, Translate 2  6 bR3
                       , Translate 3  5 bR3, Translate 4  4 bR3
                       , Translate 5  3 bR3, Translate 6  2 bR3
                       , Translate 6  3 bR3, Translate 6  4 bR3
                       , Translate 7  2 bR3, Translate 7  3 bR3
                       , Translate 7  4 bR3, Translate 8  3 bR3
                       , Translate 8  4 bR3, Translate 9  4 bR3
                       , Translate 9  5 bR3, Translate 10 5 bR3
                       , Translate 11 6 bR3, Translate 10 6 bR3
                       , Translate 11 7 bR3
                       ]
        r4  = Pictures [ Translate 2  8 bR4, Translate 3  7 bR4
                       , Translate 4  6 bR4, Translate 5  5 bR4
                       , Translate 6  5 bR4, Translate 7  5 bR4
                       , Translate 8  5 bR4, Translate 8  6 bR4
                       , Translate 9  6 bR4
                       ]
        r5  = Pictures [ Translate 3  8 bR5, Translate 4  7 bR5
                       , Translate 5  6 bR5, Translate 6  6 bR5
                       , Translate 7  6 bR5, Translate 4  9 bR5
                       , Translate 5  8 bR5, Translate 7  7 bR5
                       , Translate 8  7 bR5, Translate 9  7 bR5
                       , Translate 10 7 bR5, Translate 11 8 bR5
                       , Translate 10 9 bR5, Translate 9  8 bR5
                       , Translate 8  8 bR5, Translate 7  8 bR5
                       , Translate 8  9 bR5
                       ]
-- | HP outline sprite
outline :: Picture
outline = Pictures [ Translate 6  0  bP, Translate 5  1  bP
                       , Translate 7  1  bP, Translate 4  2  bP
                       , Translate 8  2  bP, Translate 3  3  bP
                       , Translate 9  3  bP, Translate 2  4  bP
                       , Translate 10 4  bP, Translate 1  5  bP
                       , Translate 11 5  bP, Translate 12 6  bP
                       , Translate 0  6  bP, Translate 12 7  bP
                       , Translate 0  7  bP, Translate 12 8  bP
                       , Translate 0  8  bP, Translate 1  9  bP
                       , Translate 11 9  bP, Translate 2  10 bP
                       , Translate 3  10 bP, Translate 4  10 bP
                       , Translate 8  10 bP, Translate 9  10 bP
                       , Translate 10 10 bP, Translate 7  9  bP
                       , Translate 6  8  bP, Translate 5  9  bP
                       ]
    where
        pixel = Polygon[(0,0), (0,1), (1,1), (1,0)]
        bP  = Color black pixel
-- | HP half sprite
halfHeart :: Picture
halfHeart = Pictures [r1, r2, r3, r4, r5]
    where
        pixel = Polygon[(0,0), (0,1), (1,1), (1,0)]
        bP  = Color black pixel
        bR1 = Color (makeColor 1     0.855 0.753 1) pixel
        bR2 = Color (makeColor 0.878 0     0     1) pixel
        bR3 = Color (makeColor 0.996 0     0     1) pixel
        bR4 = Color (makeColor 0.992 0.153 0.153 1) pixel
        bR5 = Color (makeColor 0.996 0.29  0.294 1) pixel
        r1  = Pictures [ Translate 2  9 bR1, Translate 3  9 bR1
                       , Translate 4  8 bR1, Translate 5  7 bR1
                       , Translate 6  7 bR1
                       ]
        r2  = Pictures [ Translate 6  1 bR2, Translate 5  2 bR2
                       , Translate 4  3 bR2, Translate 3  4 bR2
                       , Translate 2  5 bR2, Translate 1  6 bR2
                       , Translate 1  7 bR2
                       ]
        r3  = Pictures [ Translate 1  8 bR3, Translate 2  7 bR3
                       , Translate 3  6 bR3, Translate 4  5 bR3
                       , Translate 5  4 bR3, Translate 2  6 bR3
                       , Translate 3  5 bR3, Translate 4  4 bR3
                       , Translate 5  3 bR3, Translate 6  2 bR3
                       , Translate 6  3 bR3, Translate 6  4 bR3
                       ]
        r4  = Pictures [ Translate 2  8 bR4, Translate 3  7 bR4
                       , Translate 4  6 bR4, Translate 5  5 bR4
                       , Translate 6  5 bR4
                       ]
        r5  = Pictures [ Translate 3  8 bR5, Translate 4  7 bR5
                       , Translate 5  6 bR5, Translate 6  6 bR5
                       , Translate 4  9 bR5, Translate 5  8 bR5
                       ]
-- | Ammo sprite
bullet :: Picture
bullet = Pictures[s1, s3, s4, s2, s5, s6, s7, s8] 
    where
        s1 = Color (makeColor 0.757 0.616 0.043 1) $
             Pictures[ Polygon [(0,3), (2,3), (2,12), (0,12)]
                     , Polygon [(9,3), (12,3), (12,12), (9,12)]
                     , Polygon [(0,0), (12,0), (12,1), (0,1)]
                     , Polygon [(0,1), (2,1), (2,2), (0,2)]
                     , Polygon [(9,1), (12,1), (12,2), (9,2)]
                     ]
        s2 = Color (makeColor 0.31 0.251 0.02 1) $
             Pictures[ Polygon [(1,2), (11,2), (11,3), (1,3)]
                     , Polygon [(1,12), (11,12), (11,13), (1,13)]
                     ]
        s3 = Color (makeColor 0.945 0.769 0.059 1) $
             Pictures[ Polygon [(2,1), (9,1), (9,12), (2,12)]]
        s4 = Color (makeColor 0.98 0.91 0.612 1) $
             Pictures[ Polygon [(3,1), (5,1), (5,12), (3,12)]]
        s5 = Color (makeColor 0.49 0.49 0.49 1) $
             Pictures[ Polygon [(0,13), (12,13), (12,16), (0,16)]
                     , Polygon [(1,16), (11,16), (11,18), (1,18)]
                     , Polygon [(2,18), (10,18), (10,19), (2,19)]
                     , Polygon [(3,19), (9,19), (9,20), (3,20)]
                     , Polygon [(4,20), (8,20), (8,21), (4,21)]
                     ]
        s6 = Color (makeColor 0.2 0.2 0.2 1) $
             Pictures[ Polygon [(7,13), (9,13), (9,14), (7,14)]
                     , Polygon [(8,14), (10,14), (10,16), (8,16)]
                     , Polygon [(7,16), (9,16), (9,18), (7,18)]
                     , Polygon [(6,17), (8,17), (8,19), (6,19)]
                     ]
        s7 = Color (makeColor 0.8 0.8 0.8 1) $
             Pictures[ Polygon [(2,14), (3,14), (3,16), (2,16)]
                     , Polygon [(3,13), (5,13), (5,14), (3,14)]
                     , Polygon [(4,14), (5,14), (5,17), (4,17)]
                     , Polygon [(3,17), (4,17), (4,18), (3,18)]
                     , Polygon [(5,17), (6,17), (6,19), (5,19)]
                     ]
        s8 = Color (makeColor 1 1 1 1) $
             Pictures[ Polygon [(3,14), (4,14), (4,16), (3,16)]
                     , Polygon [(4,17), (5,17), (5,19), (4,19)]
                     ]
enemieL :: EnemyList
enemieL = [enemies1
          ,enemies2
          ,enemies3
          ]
mapL :: MapList
mapL = [map1
       ,map2
       ,map3
       ]

enemies1 :: Enemies
enemies1 = [Enemy (3.23,5.34) (4.21,4.61) 10 1 0.3 0.7
           ,Enemy (2.6,3.07) (2.35,1.97) 10 1 0.3 1
           ]
map1 :: GameMap
map1 = [ Wall (1,4) (1,1)  col 1
       , Wall (1,1) (3,1)  col 1
       , Wall (3,1) (3,1.6)  col 1
       , Wall (3,1.6) (1.6,1.6)  col 1
       , Wall (1.6,1.6) (1.6,3.4)  col 1
       , Wall (1.6,3.4) (3,3.4)  col 1
       , Wall (3,3.4) (3,4) col 1
       , Wall (3,4) (1,4)  col 1
       , Wall (-1,-1) (3,-1)  col 0.6
       , Wall (3,-1) (5,1)  col 0.8
       , Wall (5,1) (5,6)  col 1.2
       , Wall (5,6) (-1,6)  col 0.8
       , Wall (-1,6) (-1,-1)  col 0.6
       ]
    where
        col = makeColor (100/255) (220/255) (100/255) 1
enemies2 :: Enemies
enemies2 = [Enemy (-5,4) (-6,3) 10 1 0.3 1
           ,Enemy (-3,1.6) (-4,1.6) 10 1 0.3 1
           ,Enemy (-1,4) (0,4) 10 1 0.3 1
           ]
map2 :: GameMap
map2 = [ Wall (2,-2) (-2,-2)  col 1
       , Wall (-2,-0.6) (-2,-2)  col 1
       , Wall (-2,0.4) (-2,2)  col 1
       , Wall (-2,2) (-1,2)  col 1
       , Wall (-1,2) (-1,2.1)  col 1
       , Wall (-1,2.1) (-5,2.1)  col 1
       , Wall (2,2) (2,-2)  col 1
       , Wall (2,2) (0,2)  col 1
       , Wall (0,2) (0,2.1)  col 1
       , Wall (0,2.1) (2,2.1)  col 1
       , Wall (2,2.1) (2,5)  col 1
       , Wall (2,5) (-7,5)  col 1
       , Wall (-5,2) (-5,2.1)  col 1
       , Wall (-5,2) (-2.1,2)  col 1
       , Wall (-2.1,0.4) (-2,0.4)  col 1
       , Wall (-2.1,2) (-2.1,0.4)  col 1
       , Wall (-7,-2) (-7,2)  col 1
       , Wall (-7,2) (-6,2)  col 1
       , Wall (-6,2) (-6,2.1)  col 1
       , Wall (-6,2.1) (-7,2.1)  col 1
       , Wall (-7,2.1) (-7,5)  col 1
       , Wall (-2.1,-0.6) (-2,-0.6)  col 1
       , Wall (-2.1,-0.6) (-2.1,-2)  col 1
       , Wall (-2.1,-2) (-7,-2)  col 1
       ]
    where
        col = makeColor (210/255) (180/255) (140/255) 1
enemies3 :: Enemies
enemies3 = [ Enemy (0,4.65) (-1.74,3.25) 10 1 0.3 1
           , Enemy (6.97,5.29) (7.61,6.77) 10 1 0.3 1
           , Enemy (10.41,11.13) (12.4,11.16) 10 1 0.3 1
           , Enemy (8.62,3.59) (10,2) 10 1 0.3 1
           , Enemy (6.94,-0.83) (7.28,1.36) 10 1 0.3 1
           ]
map3 :: GameMap
map3 = [ Wall (-2.26,-1.69) (1.2,-4.14)  col 1
       , Wall (1.2,-4.14) (4.49,-3.65)  col 1
       , Wall (4.49,-3.65) (6.71,-2.36)  col 1
       , Wall (6.71,-2.36) (8.73,-4.12)  col 1
       , Wall (8.73,-4.12) (13.09,-3.81)  col 1
       , Wall (13.09,-3.81) (15.42,-0.91)  col 1
       , Wall (15.42,-0.91) (15.21,2.4)  col 1
       , Wall (15.21,2.4) (13.2,5.6)  col 1
       , Wall (13.2,5.6) (15.32,8.24)  col 1
       , Wall (15.32,8.24) (14,12)  col 1
       , Wall (14,12) (10.38,12.58)  col 1
       , Wall (10.38,12.58) (8.78,9.89)  col 1
       , Wall (8.78,9.89) (8.26,7.1)  col 1
       , Wall (8.26,7.1) (4.72,7.57)  col 1
       , Wall (4.72,7.57) (6.81,4.1)  col 1
       , Wall (6.81,4.1) (3.79,3.09)  col 1
       , Wall (3.79,3.09) (0.3,6.66)  col 1
       , Wall (0.3,6.66) (-2.86,5.63)  col 1
       , Wall (-2.86,5.63) (-4.25,2.19)  col 1
       , Wall (-4.25,2.19) (-2.26,-1.69)  col 1
       ]
    where
        col = makeColor (110/255) (100/255) (140/255) 1