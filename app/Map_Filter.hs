-- | Filters visible walls
module Map_Filter where
import Data_structures
import Constants
import Utils
import Graphics.Gloss.Geometry.Line
import Data.List
import Data.Ord
import Data.Maybe
-- | Visible walls (Far -> Close)
getFinalMap:: GameMap -> GameMap
getFinalMap = reverse . (sortOn distWall) . filterUselessWalls
-- | Filter invisible walls (behind other walls)
filterUselessWalls:: GameMap -> GameMap
filterUselessWalls m = filterOutsideViewBox $ filter (isWallVisible m) m
-- | Filter all walls outside the viewBox and crop partially visible walls
filterOutsideViewBox :: GameMap -> GameMap
filterOutsideViewBox e = map fromJust $ filter isJust (map instersectWall e)
-- | Intersect a wall with the viewBox
instersectWall:: Wall -> Maybe Wall
instersectWall w | wallOutside w && insideViewBox = Nothing
                 | insideViewBox                  = Just w
                 | otherwise                      = Just $ cropWall w
    where
        p1 = p1W w
        p2 = p2W w
        insideViewBox = not $ any (isJust) options
        options = [inter1, inter2, inter3, inter4] -- | The points where the wall intersects the viewBox
        inter1 = intersectSegSeg p1 p2 (viewBox!!0) (viewBox!!1)
        inter2 = intersectSegSeg p1 p2 (viewBox!!1) (viewBox!!2)
        inter3 = intersectSegSeg p1 p2 (viewBox!!2) (viewBox!!3)
        inter4 = intersectSegSeg p1 p2 (viewBox!!3) (viewBox!!4)
        cropWall :: Wall -> Wall -- | Gives the wall new coordinates to fit on screen
        cropWall w | wallOutside w        = w{p1W = closestOption (p1W w), p2W = closestOption (p2W w)}
                   | pointOutside (p1W w) = w{p1W = closestOption (p1W w)}
                   | pointOutside (p2W w) = w{p2W = closestOption (p2W w)}
                   | otherwise            = w
        closestOption:: Coord -> Coord -- | Choose the best new point for the wall
        closestOption p1 = head $ sortOn (distCoord p1) $ map fromJust (filter isJust options)
-- | Checks if both wall coordinates are outside viewBox
wallOutside::Wall -> Bool
wallOutside w = pointOutside (p1W w) && pointOutside (p2W w)
