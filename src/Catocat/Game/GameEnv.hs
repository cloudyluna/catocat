{-# LANGUAGE FieldSelectors #-}

module Catocat.Game.GameEnv where

import Raylib.Types


defVector2 :: Vector2
defVector2 = Vector2 0.0 0.0


defRectangle :: Rectangle
defRectangle = Rectangle 0.0 0.0 0.0 0.0


data Player = Player
    { _position :: !Vector2
    , _texture :: !(Maybe Texture)
    , _spriteFrame :: !Rectangle
    }
    deriving (Show, Eq)


makePlayer :: Vector2 -> Maybe Texture -> Rectangle -> Player
makePlayer = Player


data Direction = West | East | Stopped deriving (Show, Eq)
data PressedDownKey = A | D | NoPressedDownKey deriving (Show, Eq)


data GameEnv = GameEnv {_player :: !Player, _controller :: !PressedDownKey}


makeGameEnv :: Player -> PressedDownKey -> GameEnv
makeGameEnv = GameEnv
