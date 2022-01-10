-- | utilities
module Utils where
import Constants
import Data_structures
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Vector
import Data.Maybe
-- | The area that the player can see
viewBox :: [Coord]
viewBox = [pn1, pn2, pf2, pf1, pn1]
    where
        pn1 = (nearPlane, nearPlane * tan (degreesToRad $ -viewAngle/2))
        pn2 = (nearPlane, nearPlane * tan (degreesToRad $  viewAngle/2))
        pf1 = (farPlane , farPlane  * tan (degreesToRad $ -viewAngle/2))
        pf2 = (farPlane , farPlane  * tan (degreesToRad $  viewAngle/2))
-- | Calculate where a vector intersects a wall
wallIntercept :: Vector -> Wall -> Maybe Coord
wallIntercept v w = intersectSegSeg (0,0) v (p1W w) (p2W w)
-- | Checks if a vector intersects an enemy
enemyIntercept :: Vector -> Enemy -> Bool
enemyIntercept v e = isJust $ intersectSegSeg (0,0) v (p1E e) (p2E e)
-- | Calculate the distance to a wall
distWall:: Wall -> Float
distWall w = (abs(((x2 - x1) * y1) - (x1 * (y2 - y1)))) / (sqrt ((x2 - x1)^2 + (y2 - y1)^2)) 
    where
        (x1, y1) = p1W w
        (x2, y2) = p2W w
-- | Calculate the distance to an enemy
distEnemy:: Enemy -> Float
distEnemy = minimum . (map (distCoord (0,0))) . (flip getEnemyPoints precisionEnemyDist)
-- | Check if a wall is visible
isWallVisible:: GameMap -> Wall -> Bool
-- |isWallVisible walls w = any (id) $ map (isPointVisible filteredWalls) wallPoints
isWallVisible walls w = (length $ filter (==True) $ map (isPointVisible filteredWalls) wallPoints) > 1
    where
        wallPoints = getWallPoints w precisionWallHidden
        filteredWalls = filter (/=w) walls
-- | Check if a wall is completely in FOV
isWallFullyVisible:: GameMap -> [Enemy] -> Wall -> Bool
isWallFullyVisible walls en w = (all (id) $ map (isPointVisible filteredWalls) wallPoints)
                             && (all (id) $ map (isPointVisibleEnemy en) wallPoints)
    where
        wallPoints = getWallPoints w precisionWallHidden
        filteredWalls = filter (/=w) walls
-- | Check if a enemy is visible
isEnemyVisible:: GameMap -> Enemy -> Bool
isEnemyVisible walls e = any (id) $ map (isPointVisible walls) $ getEnemyPoints e precisionWallHidden
-- | Check if an enemy is completely in FOV
isEnemyFullyVisible:: GameMap -> [Enemy] -> Enemy -> Bool
isEnemyFullyVisible walls en e = (all (id) $ map (isPointVisible walls) $ getEnemyPoints e precisionWallHidden)
                              -- |&& (all (id) $ map (isPointVisibleEnemy filteredEnemies) $ getEnemyPoints e precisionWallHidden)
    where
        filteredEnemies = filter (/=e) en
-- | Checks if a point is visible
isPointVisible:: [Wall] -> Coord -> Bool
isPointVisible w p = not $ any isJust $ map (wallIntercept p) w
-- | Checks if a point is covered by an enemy
isPointVisibleEnemy:: [Enemy] -> Coord -> Bool
isPointVisibleEnemy e p = not $ any (id) $ map (enemyIntercept p) e
-- | N points along a wall
getWallPoints:: Wall -> Float -> [Coord]
getWallPoints w nP = getLinePoints (p1W w) (p2W w) nP
-- | N points along an enemy
getEnemyPoints:: Enemy -> Float -> [Coord]
getEnemyPoints e nP = getLinePoints (p1E e) (p2E e) nP 
-- | N points between two Coordinates
getLinePoints:: Coord -> Coord -> Float -> [Coord]
getLinePoints p1 p2 nP = map (calcVec p1 vec step) [0.. nPd]
    where
        nPd = nP * (distCoord p1 p2)
        step = (distCoord p1 p2) / nPd
        vec  = unitVetor p1 p2
        calcVec :: Coord -> Coord -> Float -> Float -> Coord
        calcVec (x, y) (vx, vy) f n = (x + vx * f * n, y + vy * f * n)
-- | Normalizing the vector
unitVetor:: Coord -> Coord -> Vector
unitVetor (x1, y1) (x2, y2) = normalizeV (x2 - x1, y2 - y1)
-- | Check if a point is outside the viewBox
pointOutside::Coord -> Bool
pointOutside (x, y) = x >= farPlane
                  ||  x <= nearPlane
                  || (y <= slope1 * x + offset1)
                  || (y >= slope2 * x + offset2)
    where
        slope1 = slope (viewBox!!3) (viewBox!!0)
        slope2 = slope (viewBox!!1) (viewBox!!2)
        offset1 = offset (viewBox!!3) slope1
        offset2 = offset (viewBox!!1) slope2
        slope:: Coord -> Coord -> Float  
        slope (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1) 
        offset:: Coord -> Float -> Float
        offset (x, y) m = y - m * x
-- | Distance between 2 points
distCoord:: Coord -> Coord -> Float
distCoord (x0, y0) (x1, y1) = sqrt((x1 - x0)^2 + (y1 - y0)^2)
-- | Sum of vectors
sumVec:: [Vector] -> Vector
sumVec [] = (0,0)
sumVec ((x,y):t) = (x + tx, y + ty)
    where
        (tx, ty) = sumVec t
-- | Rotates a point around the player
rotatePoint:: Float -> Coord -> Coord
rotatePoint angDegre (x,y) = ((x * cos ang - y * sin ang), (y * cos ang + x * sin ang))
    where
        ang = degreesToRad angDegre
-- | Move a point by a vector
movePoint:: Vector -> Coord -> Coord
movePoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
-- | Convert angular to cartesian
vetAngToCoord:: (Float, Float) -> Vector
vetAngToCoord (a,n) = (x, y)
            where
                x = n * cos   (degreesToRad a)
                y = - n * sin (degreesToRad a)
-- | Degrees to rad
degreesToRad:: Float -> Float
degreesToRad x = x * pi / 180

-- | Get contrat color
contrastColor:: Color -> Color
contrastColor c | c == black = white
                | otherwise  = black
