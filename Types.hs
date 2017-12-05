module Types where
-- (Color, Side, Piece, Face, VJRubiksCube,solvedRubiksCube,solvedCube) where

import Prelude hiding (Left, Right)
import System.Random
import Data.List

import Diagrams.Core.Types
import Diagrams.Path
import Diagrams.Prelude hiding (Color, up, up', down, down', left, left', right, right', front, front', back, back')
import Diagrams.Backend.SVG.CmdLine

import Diagrams.RubiksCube.Model hiding (Side)
import Diagrams.RubiksCube.Draw hiding (solvedRubiksCube)
import Diagrams.RubiksCube.Move

data Color
  = Blue | Green | Orange | Red | White | Yellow
  deriving (Eq, Show,Ord)

data Side
  = Back | Front | Down | Up | Left | Right
  deriving (Eq, Show)

type Piece = Color

type Face = (Side, [Piece])

type VJRubiksCube = [Face]

solvedRubiksCube :: RubiksCube (Colour Double)
solvedRubiksCube = RubiksCube (Cube f b l r u d)
  where
    f = pure orange
    b = pure red
    l = pure green
    r = pure blue
    u = pure yellow
    d = pure white

solvedCube :: VJRubiksCube
solvedCube = 
  [(Down,[White,White,White,White,White,White,White,White,White]),
  (Up,[Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow]),
  (Left,[Green,Green,Green,Green,Green,Green,Green,Green,Green]),
  (Right,[Blue,Blue,Blue,Blue,Blue,Blue,Blue,Blue,Blue]),
  (Front,[Orange,Orange,Orange,Orange,Orange,Orange,Orange,Orange,Orange]),
  (Back,[Red,Red,Red,Red,Red,Red,Red,Red,Red])]