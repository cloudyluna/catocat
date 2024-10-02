module Catocat.Game.Message where

import Raylib.Types

import Catocat.Game.GameEnv


data Message = Ignore | QuitGame deriving (Show, Eq)
