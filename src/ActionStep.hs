module ActionStep
( actionStep
) where

import Apecs 
import SDL 

import Components
import Common
import Characters

-- System called whenever the player performs a proper action
actionStep :: System' ()
actionStep = do
  writeExamines
  pure ()

-- Place important information into examine messages
writeExamines :: System' ()
writeExamines = 
  cmap(\(Character n h stats cbStats a) -> Examine $ 
    n ++ ": " ++ show a ++ ", Health: " ++ show h ++ "/" ++ show (maxHealth cbStats))

