{-# LANGUAGE TemplateHaskell #-}

module Catocat.Game.GameEnv where

import Catocat.Prelude (makeLenses)
import Catocat.Prelude.Engine (
    Rectangle,
    Texture,
    Vector2,
 )


data SpriteFrame = SpriteFrame
    { _bound :: !Rectangle
    , _counter :: !Int
    , _current :: !Int
    }
    deriving (Show, Eq)


makeLenses ''SpriteFrame


makeSpriteFrame :: Rectangle -> Int -> Int -> SpriteFrame
makeSpriteFrame = SpriteFrame


data Player = Player
    { _position :: !Vector2
    , _texture :: !(Maybe Texture)
    , _spriteFrame :: !SpriteFrame
    }
    deriving (Show, Eq)


makeLenses ''Player


makePlayer :: Vector2 -> Maybe Texture -> SpriteFrame -> Player
makePlayer = Player


data Direction = West | East | Stopped deriving (Show, Eq)


-- | High-level abstraction for device input.
data Controller = Controller
    { _ctrlUp :: !Bool
    , _ctrlDown :: !Bool
    , _ctrlLeft :: !Bool
    , _ctrlRight :: !Bool
    , _ctrlQuit :: !Bool
    }
    deriving (Show, Eq)


makeLenses ''Controller


makeController :: Bool -> Bool -> Bool -> Bool -> Bool -> Controller
makeController = Controller


defController :: Controller
defController = makeController False False False False False


data GameRunningState = Running | Pause | Quit deriving (Show, Eq)


data GameEnv = GameEnv
    { _player :: !Player
    , _controller :: !Controller
    , _runningState :: !GameRunningState
    }


makeLenses ''GameEnv


makeGameEnv :: Player -> Controller -> GameRunningState -> GameEnv
makeGameEnv = GameEnv
