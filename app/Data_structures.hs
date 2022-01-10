-- | Data strucures
module Data_structures where
import Graphics.Gloss.Data.Color
-- | Game state for storing all game info
data Game_State = Game_State
                { gameMap  :: GameMap
                , enemies  :: Enemies
                , player   :: Player
                , actions  :: Actions
                , level    :: Int
                , menu     :: MenuPos
                , winSize  :: (Int, Int)
                , aux      :: Int
                }
-- | Stores if the player is still playing or has lost
data MenuPos = MenuPlay
             | MenuGameOver
             | MenuGameWin
             deriving Eq
-- | Pair of coordinates
type Coord = (Float, Float)
-- | Storing wall position and color
data Wall = Wall
                { p1W    :: Coord
                , p2W    :: Coord
                , wColor :: Color
                , wh     :: Float
                } deriving (Show, Eq)
-- | Defining the game map as a list of walls
type GameMap = [Wall]
-- | Defineing a list of maps
type MapList = [GameMap]
-- | Storing enemy positions, HP and damage
data Enemy = Enemy
                { p1E    :: Coord
                , p2E    :: Coord
                , hpE    :: Float
                , dpsE   :: Float
                , rangeE :: Float
                , eh     :: Float
                } deriving (Show, Eq)
-- | Defineing a list of enemies
type Enemies = [Enemy]
-- | Defineing a list of lists of enemies
type EnemyList = [Enemies]
-- | Player's info
data Player = Player
                { xMove   :: Float
                , range   :: Float
                , ammo    :: Int
                , damage  :: Float
                , hpP     :: Float
                }
-- | Player's posible actions
data Actions = Actions
                { walkF    :: Bool
                , walkB    :: Bool
                , walkL    :: Bool
                , walkR    :: Bool
                , shoot    :: Bool
                , run      :: Bool
                , changeL  :: Bool
                , reload   :: Bool
                }
