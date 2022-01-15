-- | Main 11
module Main where
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Data_structures
import React_Time
import React_Event
import Draw_State
import Constants

main :: IO ()
main = do initial <- initialState
          game initial

game:: Game_State -> IO()
game initial = play
        FullScreen
        (greyN 0.8)
        60
        initial
        drawState
        reactEvent
        reactTime

initialState :: IO Game_State
initialState = do  
                let defaultPlayer = Player{ xMove   = 0
                                          , range   = 10
                                          , ammo    = 10
                                          , damage  = 6
                                          , hpP     = maximumHealth
                                          }
                let defaultAction = Actions False False False False False False False False
                return Game_State { gameMap    = mapL !! 0
                              , enemies = enemieL !! 0
                              , lights  = lightL !! 0
                              , player  = defaultPlayer
                              , actions = defaultAction
                              , level   = 0
                              , menu    = MenuPlay
                              , winSize = (0,0)
                              , aux     = 0
                              }

