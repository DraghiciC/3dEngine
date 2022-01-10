-- | Filter enemies
module Enemy_Filter where
import Data_structures
import Constants
import Utils
import Graphics.Gloss.Geometry.Line
import Data.List
import Data.Ord
import Data.Maybe
-- | Visible enemies (Far -> Close)
getFinalEnemies:: GameMap -> Enemies -> Enemies
getFinalEnemies m = reverse . (sortOn distEnemy) . (filterUselessEnemies m)
-- | Filter invisible enemies (behind walls)
filterUselessEnemies:: GameMap -> Enemies -> Enemies
filterUselessEnemies m e = filterOutsideViewBoxEnemy $ filter (isEnemyVisible m) e
-- | Filter enemies outisde the viewbox and crop partially visible enemies
filterOutsideViewBoxEnemy :: Enemies -> Enemies
filterOutsideViewBoxEnemy e = map fromJust $ filter isJust (map instersectEnemy e)
-- | Intersect a given enemy with the viewBox
instersectEnemy:: Enemy -> Maybe Enemy
instersectEnemy e | enemyOutside e && insideViewBox = Nothing             -- | Enemy outisde -> returns nothing
                  | insideViewBox                   = Just e              -- | Enemy completely inside -> return enemy
                  | otherwise                       = Just (cropEnemy e)-- | Enemy partially inside -> crops and returns enemy
    where
        p1 = p1E e
        p2 = p2E e
        insideViewBox = not $ any (isJust) options
        options = [inter1, inter2, inter3, inter4] -- | The points where the enemy intersects the viewBox
        inter1 = intersectSegSeg p1 p2 (viewBox!!0) (viewBox!!1)
        inter2 = intersectSegSeg p1 p2 (viewBox!!1) (viewBox!!2)
        inter3 = intersectSegSeg p1 p2 (viewBox!!2) (viewBox!!3)
        inter4 = intersectSegSeg p1 p2 (viewBox!!3) (viewBox!!4)
        cropEnemy :: Enemy -> Enemy -- | Gives the enemy new coordinates to fit on screen
        cropEnemy e | (pointOutside p1) && (pointOutside p2) = e{p1E = closestOption p1, p2E = closestOption p2}
                      | pointOutside p1 = e{p1E = closestOption p1}
                      | pointOutside p2 = e{p2E = closestOption p2}
                      | otherwise       = e
        closestOption:: Coord -> Coord -- | Choose the best new point for the enemy
        closestOption p1 = head $ sortOn (distCoord p1) $ map fromJust (filter isJust options)
-- | Checks if both points of the enemy are outside the viewBox
enemyOutside::Enemy -> Bool
enemyOutside e = (pointOutside (p1E e)) && (pointOutside (p2E e))

