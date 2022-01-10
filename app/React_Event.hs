-- | React event
module React_Event where

import Data_structures
import Constants
import Graphics.Gloss.Interface.Pure.Game
-- | Reacting to keyboard presses
reactEvent :: Event -> Game_State -> Game_State
reactEvent (EventResize size)                        e = e{winSize = size}
reactEvent (EventKey (Char 'w')               s _ _) e = e{actions = (actions e){walkF    = (s == Down) }}
reactEvent (EventKey (Char 's')               s _ _) e = e{actions = (actions e){walkB    = (s == Down) }}
reactEvent (EventKey (Char 'a')               s _ _) e = e{actions = (actions e){walkL    = (s == Down) }}
reactEvent (EventKey (Char 'd')               s _ _) e = e{actions = (actions e){walkR    = (s == Down) }}
reactEvent (EventKey (Char 'r')               s _ _) e = e{actions = (actions e){reload   = (s == Down) }}
reactEvent (EventKey (Char '1')               s _ _) e = e{actions = (actions e){changeL  = True}, level = 0}
reactEvent (EventKey (Char '2')               s _ _) e = e{actions = (actions e){changeL  = True}, level = 1}
reactEvent (EventKey (SpecialKey KeyShiftL)   s _ _) e = e{actions = (actions e){run      = (s == Down) }}
reactEvent (EventKey (SpecialKey KeyEnter)    s _ _) e = e{actions = (actions e){shoot    = (s == Down) }}
reactEvent (EventKey (SpecialKey KeyLeft)  Down _ _) e = e{player  = (player  e){xMove    = -arrowRotationSpeed}}
reactEvent (EventKey (SpecialKey KeyLeft)  Up   _ _) e = e{player  = (player  e){xMove    = 0}}
reactEvent (EventKey (SpecialKey KeyRight) Down _ _) e = e{player  = (player  e){xMove    = arrowRotationSpeed}}
reactEvent (EventKey (SpecialKey KeyRight) Up   _ _) e = e{player  = (player  e){xMove    = 0}}
reactEvent _ e = e
