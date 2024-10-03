{-# LANGUAGE FieldSelectors #-}

module Catocat.Game.GameEnv where

import Raylib.Types


defRectangle :: Rectangle
defRectangle = Rectangle 0.0 0.0 0.0 0.0


data SpriteFrame = SpriteFrame
    { _bound :: !Rectangle
    , _counter :: !Int
    , _current :: !Int
    }
    deriving (Show, Eq)


makeSpriteFrame :: Rectangle -> Int -> Int -> SpriteFrame
makeSpriteFrame = SpriteFrame


data Player = Player
    { _position :: !Vector2
    , _texture :: !(Maybe Texture)
    , _spriteFrame :: !SpriteFrame
    }
    deriving (Show, Eq)


makePlayer :: Vector2 -> Maybe Texture -> SpriteFrame -> Player
makePlayer = Player


data Direction = West | East | Stopped deriving (Show, Eq)


data Controller = Controller
    { _ctrlUp :: !Bool
    , _ctrlDown :: !Bool
    , _ctrlLeft :: !Bool
    , _ctrlRight :: !Bool
    }
    deriving (Show, Eq)


makeController :: Bool -> Bool -> Bool -> Bool -> Controller
makeController = Controller


defController :: Controller
defController = makeController False False False False


data GameEnv = GameEnv {_player :: !Player, _controller :: !Controller}


makeGameEnv :: Player -> Controller -> GameEnv
makeGameEnv = GameEnv
