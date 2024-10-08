module Catocat.Game.Message where

import Catocat.Prelude


data Scene = Menu | World deriving (Show, Eq)


data Message = Ignore | SwitchScene Scene | QuitGame deriving (Show, Eq)
