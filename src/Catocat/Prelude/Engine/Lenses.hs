module Catocat.Prelude.Engine.Lenses (
    v2'x,
    v2'y,
    rect'x,
    rect'y,
    rect'width,
    rect'height,
    txt'width,
) where

import Optics.Lens (Lens, lensVL)
import Raylib.Types.Core
import Raylib.Types.Core.Textures (Texture)
import Raylib.Util.Lenses


v2'x :: Lens Vector2 Vector2 Float Float
v2'x = lensVL _vector2'x


v2'y :: Lens Vector2 Vector2 Float Float
v2'y = lensVL _vector2'y


rect'x :: Lens Rectangle Rectangle Float Float
rect'x = lensVL _rectangle'x


rect'y :: Lens Rectangle Rectangle Float Float
rect'y = lensVL _rectangle'y


rect'width :: Lens Rectangle Rectangle Float Float
rect'width = lensVL _rectangle'width


rect'height :: Lens Rectangle Rectangle Float Float
rect'height = lensVL _rectangle'height


txt'width :: Lens Texture Texture Int Int
txt'width = lensVL _texture'width
