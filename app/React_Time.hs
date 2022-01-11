-- | Changes mde every frame
module React_Time where
import Data_structures
import Constants
import Utils
import Enemy_Filter
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Vector
import Data.Maybe
import Data.List
reactTime :: Float -> Game_State -> Game_State
reactTime tick e | (menu e) == MenuPlay = changeLevel tick $ nextLevel tick $reloadAmmo tick $ shootEnemy tick $ enemyDps tick $ moveWorld tick $ shootEnemy tick e
                 | otherwise            = e
-- | Change level
changeLevel :: Float -> Game_State -> Game_State
changeLevel tick e | changeL (actions e) && length mapL < (1+ level e) = e{menu = MenuGameWin}
                   | changeL (actions e)    = e{ actions = (actions e) {changeL = False}
                                            , player = (player e) {ammo = 10, hpP = maximumHealth}
                                            , gameMap    = mapL !! level e
                                            , enemies = enemieL !! level e
                                            , lights =   lightL !! level e
                                            }
                   | otherwise              = e
-- | Next level when all enemies are dead
nextLevel :: Float -> Game_State -> Game_State
nextLevel tick e | enemies e == [] = e{ actions = (actions e) {changeL = True}
                                     , level = 1 + level e
                                     }
                 | otherwise       = e
-- | Reload
reloadAmmo :: Float -> Game_State -> Game_State
reloadAmmo tick e | reload (actions e) && ammo (player e) == 0 = e{ actions = (actions e) {reload = False}
                                                                  , player = (player e) {ammo = 10}
                                                                  }
                  | reload (actions e)                         = e{ actions = (actions e) {reload = False}
                                                                  , aux = 1 + aux e
                                                                  }
                  | otherwise                                  = e
-- | Shoot a bullet
shootEnemy :: Float -> Game_State -> Game_State
shootEnemy tick e | shoot (actions e) && ammo (player e) > 0 = e{ actions = (actions e){shoot = False}
                                                                , enemies = newEnemies
                                                                , player = (player e){ammo = ammo (player e) - 1}
                                                                }
                  | otherwise                                = e
    where
        newEnemies = upEnemies ++ lineInvEn ++ outEn
        (lineEn, outEn) = partition (enemyIntercept (range(player e),0)) $ enemies e -- | Check if the enemy is intercepting the bullet
        (lineVEn, lineInvEn) = partition (isEnemyVisible (gameMap e)) lineEn -- | Check if the enemies intercepting the bullet are visible
        upEnemies = damageEnemy e $ sortOn distEnemy lineVEn -- | The closest enemy that is visible and is intercepting the bullet takes damage
        damageEnemy :: Game_State -> [Enemy] -> [Enemy] -- | damage the enemy
        damageEnemy _ [] = []
        damageEnemy e (en:t) | newHP <= 0 = t
                             | otherwise  = en{hpE = newHP}:t
            where
                newHP = (hpE en) - damage(player e)
-- | Movement
moveWorld:: Float -> Game_State -> Game_State
moveWorld tick e | interWall || interEnemy = rotateLights tick $ rotateEnemies tick $ rotateMap tick e -- | If hitting a wall or enemt then only rotate
                 | otherwise               = rotateLights tick $ moveLights tick $ rotateEnemies tick $ moveEnemies tick $ rotateMap tick $ moveMap tick e -- | Rotate and/or move
    where
        (vx, vy) = getVecTranslate tick e
        interWall  = any isJust $ map (wallIntercept (0,0) (-vx, -vy)) (gameMap e) -- | Checks if the player is trying to move throught a wall
        interEnemy = length (filter (enemyIntercept (-vx, -vy)) (enemies e)) > 0 -- | Checks if the player is trying to move throught an enemy
-- | Damage from enemies
enemyDps :: Float -> Game_State -> Game_State
enemyDps tick e | newHP <= 0 = e{player = (player e){hpP = 0}, menu = MenuGameOver}
                | otherwise  = e{player = (player e){hpP = newHP}}
    where
        newHP = hpP (player e) - closeEnemiesDPS * tick
        closeEnemiesDPS = sum $ map dpsE $ filter inRangeEnemy  (enemies e)
        inRangeEnemy:: Enemy -> Bool
        inRangeEnemy e = (distEnemy e) <= (rangeE e)
-- | Rotate all lights
rotateLights:: Float -> Game_State -> Game_State
rotateLights tick e = e{lights = map (rotateLight (tick * (xMove $ player e))) (lights e)}
-- | Rotate a light
rotateLight::Float -> Coord -> Coord
rotateLight a l = rotatePoint a l
-- | Translate all lights
moveLights::Float -> Game_State -> Game_State
moveLights tick e = e{lights = map (moveLight $ getVecTranslate tick e) (lights e)}
-- | Translate a light by a vector
moveLight:: Vector -> Coord -> Coord
moveLight v l = movePoint v l

-- | Rotate all walls
rotateMap:: Float -> Game_State -> Game_State
rotateMap tick e = e{gameMap = map (rotateWall (tick * (xMove $ player e))) (gameMap e)}
-- | Rotate a wall
rotateWall::Float -> Wall -> Wall
rotateWall a w = w{p1W = rotatePoint a (p1W w), p2W = rotatePoint a (p2W w)}
-- | Translate all walls
moveMap::Float -> Game_State -> Game_State
moveMap tick e = e{gameMap = map (moveWall $ getVecTranslate tick e) (gameMap e)}
-- | Translate a wall by a vector
moveWall:: Vector -> Wall -> Wall
moveWall v w = w{p1W = movePoint v (p1W w), p2W = movePoint v (p2W w)}
-- | Rotate all enemies
rotateEnemies:: Float -> Game_State -> Game_State
rotateEnemies tick e = e{enemies = map (rotateEnemy rX) (enemies e)}
                    where
                      rX = tick * (xMove $ player e)
-- | Rotate an given enemy
rotateEnemy::Float -> Enemy -> Enemy
rotateEnemy a e = e{p1E = rotatePoint a (p1E e), p2E = rotatePoint a (p2E e)}
-- | Translate all enemies
moveEnemies:: Float -> Game_State -> Game_State
moveEnemies tick e = e{enemies = map (moveEnemy $ getVecTranslate tick e) (enemies e)}
-- | Translate a Enemy by a vector
moveEnemy:: Vector -> Enemy -> Enemy
moveEnemy v e = e{p1E = movePoint v (p1E e), p2E = movePoint v (p2E e)}
-- | Player's movement vector
getVecTranslate::Float -> Game_State -> (Float, Float)
getVecTranslate tick e = (vx * dist, vy * dist) 
    where
        dist = tick * if run(actions e) then runSpeed else walkSpeed
        vt = sumVec[wL, wR, wF, wB]
        (vx, vy) = if (vt == (0,0)) then vt else normalizeV vt
        wL   = if walkL $ actions e    then ( 0, -1) else (0, 0)
        wR   = if walkR $ actions e    then ( 0,  1) else (0, 0)
        wF   = if walkF $ actions e    then (-1,  0) else (0, 0)
        wB   = if walkB $ actions e    then ( 1,  0) else (0, 0)
