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
  deriving (Eq, Show)

data Side
  = Back | Front | Down | Top | Left | Right
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

getColorOfSide :: Side -> Color
getColorOfSide side = case side of
  Top   -> Yellow
  Front -> Orange
  Left  -> Green
  Back  -> Red
  Right -> Blue
  Down  -> White

-- Moves
replaceNth :: Int -> Color -> [Color] -> [Color]
replaceNth n newVal (x:xs)
  | n == 0    = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs
   
getNth :: Int -> [Color] -> Color
getNth n (x:xs) 
  | n == 0    = x
  | otherwise = getNth (n-1) xs

moveThree :: Int -> Int -> Int -> Int -> Int -> Int -> [Color] -> [Color] -> [Color]
moveThree s1 s2 s3 d1 d2 d3 src dst = 
  let a = replaceNth d1 (src!!s1) dst in
    let a' = replaceNth d2 (src!!s2) a in
      let a'' = replaceNth d3 (src!!s3) a' in
        a''   

getPieces :: Maybe [Piece] -> [Piece]
getPieces pieces =
  case pieces of
    Nothing -> []
    Just p  -> p

up :: VJRubiksCube -> VJRubiksCube
up cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Top cube) in
  let l = getPieces (lookup Left cube) in
  let r = getPieces (lookup Right cube) in
  let d = getPieces (lookup Down cube) in
  let b' = b in
  let f' = f in
  let t' = t in
  let l' = l in
  let r' = r in
  let f'' = moveThree 0 1 2 0 1 2 r f' in
  let l'' = moveThree 0 1 2 0 1 2 f l' in
  let b'' = moveThree 0 1 2 0 1 2 l b' in
  let r'' = moveThree 0 1 2 0 1 2 b r' in
  let t'' = moveThree 0 1 2 2 5 8 t t' in
  let t''' = moveThree 3 4 5 1 4 7 t t'' in
  let t'''' = moveThree 6 7 8 0 3 6 t t''' in
    [(Front, f''), (Back, b''), (Left, l''), (Right, r''), (Top, t''''), (Down, d)]

up' :: VJRubiksCube -> VJRubiksCube
up' cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Top cube) in
  let l = getPieces (lookup Left cube) in
  let r = getPieces (lookup Right cube) in
  let d = getPieces (lookup Down cube) in
  let b' = b in
  let f' = f in
  let t' = t in
  let l' = l in
  let r' = r in
  let f'' = moveThree 0 1 2 0 1 2 l f' in
  let l'' = moveThree 0 1 2 0 1 2 b l' in
  let b'' = moveThree 0 1 2 0 1 2 r b' in
  let r'' = moveThree 0 1 2 0 1 2 f r' in
  let t'' = moveThree 0 1 2 6 3 0  t t' in
  let t''' = moveThree 3 4 5 7 4 1 t t'' in
  let t'''' = moveThree 6 7 8 8 5 2 t t''' in
    [(Front, f''), (Back, b''), (Left, l''), (Right, r''), (Top, t''''), (Down, d)]

down :: VJRubiksCube -> VJRubiksCube
down cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Top cube) in
  let l = getPieces (lookup Left cube) in
  let r = getPieces (lookup Right cube) in
  let d = getPieces (lookup Down cube) in
  let b' = b in
  let f' = f in
  let d' = d in
  let l' = l in
  let r' = r in
  let f'' = moveThree 6 7 8 6 7 8 l f' in
  let l'' = moveThree 6 7 8 6 7 8 b l' in
  let b'' = moveThree 6 7 8 6 7 8 r b' in
  let r'' = moveThree 6 7 8 6 7 8 f r' in
  let d'' = moveThree 0 1 2 2 5 8 d d' in
  let d''' = moveThree 3 4 5 1 4 7 d d'' in
  let d'''' = moveThree 6 7 8 0 3 6 d d''' in
    [(Front, f''), (Back, b''), (Left, l''), (Right, r''), (Top, t), (Down, d'''')]

down' :: VJRubiksCube -> VJRubiksCube
down' cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Top cube) in
  let l = getPieces (lookup Left cube) in
  let r = getPieces (lookup Right cube) in
  let d = getPieces (lookup Down cube) in
  let b' = b in
  let f' = f in
  let d' = d in
  let l' = l in
  let r' = r in
  let f'' = moveThree 6 7 8 6 7 8 r f' in
  let l'' = moveThree 6 7 8 6 7 8 f l' in
  let b'' = moveThree 6 7 8 6 7 8 l b' in
  let r'' = moveThree 6 7 8 6 7 8 b r' in
  let d'' = moveThree 0 1 2 6 3 0 d d' in
  let d''' = moveThree 3 4 5 7 4 1 d d'' in
  let d'''' = moveThree 6 7 8 8 5 2 d d''' in
    [(Front, f''), (Back, b''), (Left, l''), (Right, r''), (Top, t), (Down, d'''')]

left :: VJRubiksCube -> VJRubiksCube
left cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Top cube) in
  let l = getPieces (lookup Left cube) in
  let r = getPieces (lookup Right cube) in
  let d = getPieces (lookup Down cube) in
  let b' = b in
  let f' = f in
  let d' = d in
  let l' = l in
  let t' = t in
  let f'' = moveThree 0 3 6 0 3 6 t f' in
  let t'' = moveThree 8 5 2 0 3 6 b t' in
  let b'' = moveThree 6 3 0 2 5 8 d b' in
  let d'' = moveThree 0 3 6 0 3 6 f d' in
  let l'' = moveThree 0 1 2 2 5 8 l l' in
  let l''' = moveThree 3 4 5 1 4 7 l l'' in
  let l'''' = moveThree 6 7 8 0 3 6 l l''' in
    [(Front, f''), (Back, b''), (Left, l''''), (Right, r), (Top, t''), (Down, d'')]

left' :: VJRubiksCube -> VJRubiksCube
left' cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Top cube) in
  let l = getPieces (lookup Left cube) in
  let r = getPieces (lookup Right cube) in
  let d = getPieces (lookup Down cube) in
  let b' = b in
  let f' = f in
  let d' = d in
  let l' = l in
  let t' = t in
  let f'' = moveThree 0 3 6 0 3 6 d f' in
  let t'' = moveThree 0 3 6 0 3 6 f t' in
  let b'' = moveThree 6 3 0 2 5 8 t b' in
  let d'' = moveThree 8 5 2 0 3 6 b d' in
  let l'' = moveThree 0 1 2 6 3 0 l l' in
  let l''' = moveThree 3 4 5 7 4 1 l l'' in
  let l'''' = moveThree 6 7 8 8 5 2 l l''' in
    [(Front, f''), (Back, b''), (Left, l''''), (Right, r), (Top, t''), (Down, d'')]

right :: VJRubiksCube -> VJRubiksCube
right cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Top cube) in
  let l = getPieces (lookup Left cube) in
  let r = getPieces (lookup Right cube) in
  let d = getPieces (lookup Down cube) in
  let b' = b in
  let f' = f in
  let d' = d in
  let r' = r in
  let t' = t in
  let f'' = moveThree 2 5 8 2 5 8 d f' in
  let t'' = moveThree 2 5 8 2 5 8 f t' in
  let b'' = moveThree 8 5 2 0 3 6 t b' in
  let d'' = moveThree 6 3 0 2 5 8 b d' in
  let r'' = moveThree 0 1 2 2 5 8 r r' in
  let r''' = moveThree 3 4 5 1 4 7 r r'' in
  let r'''' = moveThree 6 7 8 0 3 6 r r''' in
    [(Front, f''), (Back, b''), (Left, l), (Right, r''''), (Top, t''), (Down, d'')]

right' :: VJRubiksCube -> VJRubiksCube
right' cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Top cube) in
  let l = getPieces (lookup Left cube) in
  let r = getPieces (lookup Right cube) in
  let d = getPieces (lookup Down cube) in
  let b' = b in
  let f' = f in
  let d' = d in
  let r' = r in
  let t' = t in
  let f'' = moveThree 2 5 8 2 5 8 t f' in
  let t'' = moveThree 6 3 0 2 5 8 b t' in
  let b'' = moveThree 8 5 2 0 3 6 d b' in
  let d'' = moveThree 2 5 8 2 5 8 f d' in
  let r'' = moveThree 0 1 2 6 3 0 r r' in
  let r''' = moveThree 3 4 5 7 4 1 r r'' in
  let r'''' = moveThree 6 7 8 8 5 2 r r''' in
    [(Front, f''), (Back, b''), (Left, l), (Right, r''''), (Top, t''), (Down, d'')]

front :: VJRubiksCube -> VJRubiksCube
front cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Top cube) in
  let l = getPieces (lookup Left cube) in
  let r = getPieces (lookup Right cube) in
  let d = getPieces (lookup Down cube) in
  let l' = l in
  let f' = f in
  let d' = d in
  let r' = r in
  let t' = t in
  let r'' = moveThree 6 7 8 0 3 6 t r' in
  let t'' = moveThree 8 5 2 6 7 8 l t' in
  let l'' = moveThree 0 1 2 2 5 8 d l' in
  let d'' = moveThree 6 3 0 0 1 2 r d' in
  let f'' = moveThree 0 1 2 2 5 8 f f' in
  let f''' = moveThree 3 4 5 1 4 7 f f'' in
  let f'''' = moveThree 6 7 8 0 3 6 f f''' in
    [(Front, f''''), (Back, b), (Left, l''), (Right, r''), (Top, t''), (Down, d'')]

front' :: VJRubiksCube -> VJRubiksCube
front' cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Top cube) in
  let l = getPieces (lookup Left cube) in
  let r = getPieces (lookup Right cube) in
  let d = getPieces (lookup Down cube) in
  let l' = l in
  let f' = f in
  let d' = d in
  let r' = r in
  let t' = t in
  let r'' = moveThree 2 1 0 0 3 6 d r' in
  let t'' = moveThree 0 3 6 6 7 8 r t' in
  let l'' = moveThree 8 7 6 2 5 8 t l' in
  let d'' = moveThree 2 5 8 0 1 2 l d' in
  let f'' = moveThree 0 1 2 6 3 0 f f' in
  let f''' = moveThree 3 4 5 7 4 1 f f'' in
  let f'''' = moveThree 6 7 8 8 5 2 f f''' in
    [(Front, f''''), (Back, b), (Left, l''), (Right, r''), (Top, t''), (Down, d'')]      

back :: VJRubiksCube -> VJRubiksCube
back cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Top cube) in
  let l = getPieces (lookup Left cube) in
  let r = getPieces (lookup Right cube) in
  let d = getPieces (lookup Down cube) in
  let l' = l in
  let b' = b in
  let d' = d in
  let r' = r in
  let t' = t in
  let r'' = moveThree 8 7 6 2 5 8 d r' in
  let t'' = moveThree 2 5 8 0 1 2 r t' in
  let l'' = moveThree 2 1 0 0 3 6 t l' in
  let d'' = moveThree 0 3 6 6 7 8 l d' in
  let b'' = moveThree 0 1 2 2 5 8 b b' in
  let b''' = moveThree 3 4 5 1 4 7 b b'' in
  let b'''' = moveThree 6 7 8 0 3 6 b b''' in
    [(Front, f), (Back, b''''), (Left, l''), (Right, r''), (Top, t''), (Down, d'')]

back' :: VJRubiksCube -> VJRubiksCube
back' cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Top cube) in
  let l = getPieces (lookup Left cube) in
  let r = getPieces (lookup Right cube) in
  let d = getPieces (lookup Down cube) in
  let l' = l in
  let b' = b in
  let d' = d in
  let r' = r in
  let t' = t in
  let r'' = moveThree 0 1 2 2 5 8 t r' in
  let t'' = moveThree 6 3 0 0 1 2 l t' in
  let l'' = moveThree 6 7 8 0 3 6 d l' in
  let d'' = moveThree 8 5 2 6 7 8 r d' in
  let b'' = moveThree 0 1 2 6 3 0 b b' in
  let b''' = moveThree 3 4 5 7 4 1 b b'' in
  let b'''' = moveThree 6 7 8 8 5 2 b b''' in
    [(Front, f), (Back, b''''), (Left, l''), (Right, r''), (Top, t''), (Down, d'')]      

solvedCube :: VJRubiksCube
solvedCube = 
  [(Down,[White,White,White,White,White,White,White,White,White]),
  (Top,[Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow]),
  (Left,[Green,Green,Green,Green,Green,Green,Green,Green,Green]),
  (Right,[Blue,Blue,Blue,Blue,Blue,Blue,Blue,Blue,Blue]),
  (Front,[Orange,Orange,Orange,Orange,Orange,Orange,Orange,Orange,Orange]),
  (Back,[Red,Red,Red,Red,Red,Red,Red,Red,Red])]

doMove :: Int -> VJRubiksCube -> VJRubiksCube
doMove num cube = 
  case num of 
    0  -> up cube
    1  -> up' cube
    2  -> down cube
    3  -> down' cube
    4  -> left cube
    5  -> left' cube
    6  -> right cube
    7  -> right' cube
    8  -> front cube
    9  -> front' cube
    10 -> back cube
    11 -> back' cube

translateMove :: VJRubiksCube -> Side -> Int -> VJRubiksCube
translateMove cube frontSide move =
  case frontSide of
    Right -> case move of
      0  -> doMove 0 cube
      1  -> doMove 1 cube
      2  -> doMove 2 cube
      3  -> doMove 3 cube
      4  -> doMove 8 cube
      5  -> doMove 9 cube
      6  -> doMove 10 cube
      7  -> doMove 11 cube
      8  -> doMove 6 cube
      9  -> doMove 7 cube
      10 -> doMove 4 cube
      11 -> doMove 5 cube
    Back -> case move of
      0  -> doMove 0 cube
      1  -> doMove 1 cube
      2  -> doMove 2 cube
      3  -> doMove 3 cube
      4  -> doMove 6 cube
      5  -> doMove 7 cube
      6  -> doMove 4 cube
      7  -> doMove 5 cube
      8  -> doMove 10 cube
      9  -> doMove 11 cube
      10 -> doMove 8 cube
      11 -> doMove 9 cube
    Left -> case move of 
      0  -> doMove 0 cube
      1  -> doMove 1 cube
      2  -> doMove 2 cube
      3  -> doMove 3 cube
      4  -> doMove 10 cube
      5  -> doMove 11 cube
      6  -> doMove 8 cube
      7  -> doMove 9 cube
      8  -> doMove 4 cube
      9  -> doMove 5 cube
      10 -> doMove 6 cube
      11 -> doMove 7 cube
    Front -> case move of
      0  -> doMove 0 cube
      1  -> doMove 1 cube
      2  -> doMove 2 cube
      3  -> doMove 3 cube
      4  -> doMove 4 cube
      5  -> doMove 5 cube
      6  -> doMove 6 cube
      7  -> doMove 7 cube
      8  -> doMove 8 cube
      9  -> doMove 9 cube
      10 -> doMove 10 cube
      11 -> doMove 11 cube
       
addMove :: Side -> Int -> Move
addMove frontSide move = 
  case frontSide of
    Right -> case move of
      0  -> U
      1  -> U'
      2  -> D
      3  -> D'
      4  -> F
      5  -> F'
      6  -> B
      7  -> B'
      8  -> R
      9  -> R'
      10 -> L
      11 -> L'
    Back -> case move of
      0  -> U
      1  -> U'
      2  -> D
      3  -> D'
      4  -> R
      5  -> R'
      6  -> L
      7  -> L'
      8  -> B
      9  -> B'
      10 -> F
      11 -> F'
    Left -> case move of 
      0  -> U
      1  -> U'
      2  -> D
      3  -> D'
      4  -> B
      5  -> B'
      6  -> F
      7  -> F'
      8  -> L
      9  -> L'
      10 -> R
      11 -> R'
    Front -> case move of
      0  -> U
      1  -> U'
      2  -> D
      3  -> D'
      4  -> L
      5  -> L'
      6  -> R
      7  -> R'
      8  -> F
      9  -> F'
      10 -> B
      11 -> B'

scramble :: VJRubiksCube -> IO VJRubiksCube
scramble cube = do
  seed <- newStdGen
  let list = randomlist 25 seed
  let alteredCube = foldl (\c n -> doMove n c) cube list
  return alteredCube

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . randomR(0, 11))

-- Stage 1

solveTopCross :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
solveTopCross (cube, moves) = case checkExtendedCross cube of
  True  -> (cube, [])
  False -> fixTopEdges (cube, moves)

checkCross :: VJRubiksCube -> Bool
checkCross cube = 
  let topPieces = getPieces (lookup Top cube) in
    if ((getNth 1 topPieces == getColorOfSide Top) &&
        (getNth 3 topPieces == getColorOfSide Top) &&
        (getNth 5 topPieces == getColorOfSide Top) &&
        (getNth 7 topPieces == getColorOfSide Top)) then True else False

checkExtendedCross :: VJRubiksCube -> Bool
checkExtendedCross cube = 
  let frontPieces = getPieces (lookup Front cube) in
    let rightPieces = getPieces (lookup Right cube) in
      let leftPieces = getPieces (lookup Left cube) in
        let backPieces = getPieces (lookup Back cube) in
          if ((getNth 1 frontPieces == getColorOfSide Front) &&
              (getNth 1 rightPieces == getColorOfSide Right) &&
              (getNth 1 leftPieces  == getColorOfSide Left)  &&
              (getNth 1 backPieces  == getColorOfSide Back)) &&
              (checkCross cube == True) then True else False

fixTopEdges :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
fixTopEdges (cube, moves) = 
  let (cube', moves') = fixFrontTopEdges (cube, moves) (getPieces (lookup Front cube)) in
    let (cube'', moves'') = fixRightTopEdges (cube', moves') (getPieces (lookup Right cube')) in
      let (cube''', moves''') = fixLeftTopEdges (cube'', moves'') (getPieces (lookup Left cube'')) in
        fixBackTopEdges (cube''', moves''') (getPieces (lookup Back cube'''))

fixFrontTopEdges :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixFrontTopEdges (cube, moves) pieces 
  | getNth 1 pieces                        == getColorOfSide Top = fixFrontTopEdges (frontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Front) Front) (getPieces (lookup Front (fst (frontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Front) Front))))
  | getNth 3 pieces                        == getColorOfSide Top = fixFrontTopEdges (downFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Front) Front) (getPieces (lookup Front (fst (downFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Front) Front))))
  | getNth 5 pieces                        == getColorOfSide Top = fixFrontTopEdges (downFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Front) Front) (getPieces (lookup Front (fst (downFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Front) Front))))
  | getNth 7 pieces                        == getColorOfSide Top = fixFrontTopEdges (frontFaceEdgeElevationCase (cube, moves) Front) (getPieces (lookup Front (fst (frontFaceEdgeElevationCase (cube, moves) Front))))
  | getNth 1 (getPieces(lookup Down cube)) == getColorOfSide Top = fixFrontTopEdges (downFaceEdgeElevationCase (cube, moves) Front) (getPieces (lookup Front (fst (downFaceEdgeElevationCase (cube, moves) Front))))
  | otherwise                                                    = (cube, moves)

fixRightTopEdges :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixRightTopEdges (cube, moves) pieces 
  | getNth 1 pieces                        == getColorOfSide Top = fixRightTopEdges (frontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Right) Right) (getPieces (lookup Right (fst (frontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Right) Right))))
  | getNth 3 pieces                        == getColorOfSide Top = fixRightTopEdges (downFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Right) Right) (getPieces (lookup Right (fst (downFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Right) Right))))
  | getNth 5 pieces                        == getColorOfSide Top = fixRightTopEdges (downFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Right) Right) (getPieces (lookup Right (fst (downFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Right) Right))))
  | getNth 7 pieces                        == getColorOfSide Top = fixRightTopEdges (frontFaceEdgeElevationCase (cube, moves) Right) (getPieces (lookup Right (fst (frontFaceEdgeElevationCase (cube, moves) Right))))
  | getNth 5 (getPieces(lookup Down cube)) == getColorOfSide Top = fixRightTopEdges (downFaceEdgeElevationCase (cube, moves) Right) (getPieces (lookup Right (fst (downFaceEdgeElevationCase (cube, moves) Right))))
  | otherwise                                                    = (cube, moves)
 
fixLeftTopEdges :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixLeftTopEdges (cube, moves) pieces 
  | getNth 1 pieces                        == getColorOfSide Top = fixLeftTopEdges (frontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Left) Left) (getPieces (lookup Left (fst (frontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Left) Left))))
  | getNth 3 pieces                        == getColorOfSide Top = fixLeftTopEdges (downFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Left) Left) (getPieces (lookup Left (fst (downFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Left) Left))))
  | getNth 5 pieces                        == getColorOfSide Top = fixLeftTopEdges (downFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Left) Left) (getPieces (lookup Left (fst (downFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Left) Left))))
  | getNth 7 pieces                        == getColorOfSide Top = fixLeftTopEdges (frontFaceEdgeElevationCase (cube, moves) Left) (getPieces (lookup Left (fst (frontFaceEdgeElevationCase (cube, moves) Left))))
  | getNth 3 (getPieces(lookup Down cube)) == getColorOfSide Top = fixLeftTopEdges (downFaceEdgeElevationCase (cube, moves) Left) (getPieces (lookup Left (fst (downFaceEdgeElevationCase (cube, moves) Left))))
  | otherwise                                                    = (cube, moves)

fixBackTopEdges :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move]) 
fixBackTopEdges (cube, moves) pieces  
  | getNth 1 pieces                        == getColorOfSide Top = fixBackTopEdges (frontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Back) Back) (getPieces (lookup Back (fst (frontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Back) Back))))
  | getNth 3 pieces                        == getColorOfSide Top = fixBackTopEdges (downFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Back) Back) (getPieces (lookup Back (fst (downFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Back) Back))))
  | getNth 5 pieces                        == getColorOfSide Top = fixBackTopEdges (downFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Back) Back) (getPieces (lookup Back (fst (downFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Back) Back))))
  | getNth 7 pieces                        == getColorOfSide Top = fixBackTopEdges (frontFaceEdgeElevationCase (cube, moves) Back) (getPieces (lookup Back (fst (frontFaceEdgeElevationCase (cube, moves) Back))))
  | getNth 7 (getPieces(lookup Down cube)) == getColorOfSide Top = fixBackTopEdges (downFaceEdgeElevationCase (cube, moves) Back) (getPieces (lookup Back (fst (downFaceEdgeElevationCase (cube, moves) Back))))
  | otherwise                                                    = (cube, moves)

-- F, F
skillfulTwist1 :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
skillfulTwist1 (cube, moves) side =
  (translateMove (translateMove cube side 8) side 8, moves ++ [addMove side 8, addMove side 8])

-- L, D, L'
skillfulTwist3 :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
skillfulTwist3 (cube, moves) side =
  (translateMove (translateMove (translateMove cube side 4) side 2) side 5, moves ++ [addMove side 4, addMove side 2, addMove side 5])

-- R', D', R
skillfulTwist5 :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
skillfulTwist5 (cube, moves) side =
  (translateMove (translateMove (translateMove cube side 7) side 3) side 6, moves ++ [addMove side 7, addMove side 3, addMove side 6])

-- D, R, F', R'
frontFaceEdgeElevationCase :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
frontFaceEdgeElevationCase (cube, moves) side = 
  let pieces = getPieces (lookup Down cube) in
    case side of
      Front -> if (getNth 1 pieces == getColorOfSide side) then (translateMove (translateMove (translateMove (translateMove cube side 2) side 6) side 9) side 7, moves ++ [addMove side 2, addMove side 6, addMove side 9, addMove side 7])
               else (frontFaceEdgeElevationCase (down cube, moves ++ [addMove side 2]) Right)
      Right -> if (getNth 5 pieces == getColorOfSide side) then (translateMove (translateMove (translateMove (translateMove cube side 2) side 6) side 9) side 7, moves ++ [addMove side 2, addMove side 6, addMove side 9, addMove side 7])
               else (frontFaceEdgeElevationCase (down cube, moves ++ [addMove side 2]) Back)
      Back  -> if (getNth 7 pieces == getColorOfSide side) then (translateMove (translateMove (translateMove (translateMove cube side 2) side 6) side 9) side 7, moves ++ [addMove side 2, addMove side 6, addMove side 9, addMove side 7])
               else (frontFaceEdgeElevationCase (down cube, moves ++ [addMove side 2]) Left)
      Left  -> if (getNth 3 pieces == getColorOfSide side) then (translateMove (translateMove (translateMove (translateMove cube side 2) side 6) side 9) side 7, moves ++ [addMove side 2, addMove side 6, addMove side 9, addMove side 7])
               else (frontFaceEdgeElevationCase (down cube, moves ++ [addMove side 2]) Front)

-- F, F
downFaceEdgeElevationCase :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
downFaceEdgeElevationCase (cube, moves) side = 
  let pieces = getPieces (lookup side cube) in
    case side of
      Front -> if (getNth 7 pieces == getColorOfSide side) then (translateMove (translateMove cube side 8) side 8, moves ++ [addMove side 8, addMove side 8])
               else (downFaceEdgeElevationCase (down cube, moves ++ [addMove side 2]) Right)
      Right -> if (getNth 7 pieces == getColorOfSide side) then (translateMove (translateMove cube side 8) side 8, moves ++ [addMove side 8, addMove side 8])
               else (downFaceEdgeElevationCase (down cube, moves ++ [addMove side 2]) Back)
      Back  -> if (getNth 7 pieces == getColorOfSide side) then (translateMove (translateMove cube side 8) side 8, moves ++ [addMove side 8, addMove side 8])
               else (downFaceEdgeElevationCase (down cube, moves ++ [addMove side 2]) Left)
      Left  -> if (getNth 7 pieces == getColorOfSide side) then (translateMove (translateMove cube side 8) side 8, moves ++ [addMove side 8, addMove side 8])
               else (downFaceEdgeElevationCase (down cube, moves ++ [addMove side 2]) Front)


-- Stage 2

checkCorners :: VJRubiksCube -> Bool
checkCorners cube = 
  let topPieces = getPieces (lookup Top cube) in
    if ((getNth 0 topPieces == getColorOfSide Top) &&
        (getNth 2 topPieces == getColorOfSide Top) &&
        (getNth 6 topPieces == getColorOfSide Top) &&
        (getNth 8 topPieces == getColorOfSide Top)) then True else False

checkExtendedCorners :: VJRubiksCube -> Bool
checkExtendedCorners cube = 
  let frontPieces = getPieces (lookup Front cube) in
    let rightPieces = getPieces (lookup Right cube) in
      let leftPieces = getPieces (lookup Left cube) in
        let backPieces = getPieces (lookup Back cube) in
          if ((getNth 0 frontPieces == getColorOfSide Front) &&
              (getNth 2 frontPieces == getColorOfSide Front) &&
              (getNth 0 rightPieces == getColorOfSide Right) &&
              (getNth 2 rightPieces == getColorOfSide Right) &&
              (getNth 0 leftPieces  == getColorOfSide Left)  &&
              (getNth 2 leftPieces  == getColorOfSide Left)  &&
              (getNth 0 backPieces  == getColorOfSide Back)  &&
              (getNth 2 backPieces  == getColorOfSide Back)) &&
              (checkCorners cube == True) then True else False

solveTopCorners :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
solveTopCorners (cube, moves) = case checkExtendedCorners cube of
  True  -> (cube, moves)
  False -> fixTopCorners (cube, moves)

fixTopCorners :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
fixTopCorners (cube, moves) = 
  let (cube', moves') = fixFrontTopCorners (cube, moves) (getPieces (lookup Front cube)) in
    let (cube'', moves'') = fixRightTopCorners (cube', moves') (getPieces (lookup Right cube')) in
      let (cube''', moves''') = fixLeftTopCorners (cube'', moves'') (getPieces (lookup Left cube'')) in
        fixBackTopCorners (cube''', moves''') (getPieces (lookup Back cube'''))

fixFrontTopCorners :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixFrontTopCorners (cube, moves) pieces 
  | getNth 0 pieces                        == getColorOfSide Top = fixFrontTopCorners (leftCornerLiftingCase (skillfulTwist0 (cube, moves) Front) Front) (getPieces (lookup Front (fst (leftCornerLiftingCase (skillfulTwist0 (cube, moves) Front) Front))))
  | getNth 2 pieces                        == getColorOfSide Top = fixFrontTopCorners (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Front) Front) (getPieces (lookup Front (fst (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Front) Front))))
  | getNth 6 pieces                        == getColorOfSide Top = fixFrontTopCorners (leftCornerLiftingCase (cube, moves) Front) (getPieces (lookup Front (fst (leftCornerLiftingCase (cube, moves) Front))))
  | getNth 8 pieces                        == getColorOfSide Top = fixFrontTopCorners (rightCornerLiftingCase (cube, moves) Front) (getPieces (lookup Front (fst (rightCornerLiftingCase (cube, moves) Front))))
  | getNth 0 (getPieces(lookup Down cube)) == getColorOfSide Top = fixFrontTopCorners (topLeftHardCornerLiftingCase (cube, moves) Front) (getPieces (lookup Front (fst (topLeftHardCornerLiftingCase (cube, moves) Front))))
  | getNth 2 (getPieces(lookup Down cube)) == getColorOfSide Top = fixFrontTopCorners (topRightHardCornerLiftingCase (cube, moves) Front) (getPieces (lookup Front (fst (topRightHardCornerLiftingCase (cube, moves) Front))))
  | otherwise                                                    = (cube, moves)

fixRightTopCorners :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixRightTopCorners (cube, moves) pieces 
  | getNth 0 pieces                        == getColorOfSide Top = fixRightTopCorners (leftCornerLiftingCase (skillfulTwist0 (cube, moves) Right) Right) (getPieces (lookup Right (fst (leftCornerLiftingCase (skillfulTwist0 (cube, moves) Right) Right))))
  | getNth 2 pieces                        == getColorOfSide Top = fixRightTopCorners (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Right) Right) (getPieces (lookup Right (fst (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Right) Right))))
  | getNth 6 pieces                        == getColorOfSide Top = fixRightTopCorners (leftCornerLiftingCase (cube, moves) Right) (getPieces (lookup Right (fst (leftCornerLiftingCase (cube, moves) Right))))
  | getNth 8 pieces                        == getColorOfSide Top = fixRightTopCorners (rightCornerLiftingCase (cube, moves) Right) (getPieces (lookup Right (fst (rightCornerLiftingCase (cube, moves) Right))))
  | getNth 2 (getPieces(lookup Down cube)) == getColorOfSide Top = fixRightTopCorners (topLeftHardCornerLiftingCase (cube, moves) Right) (getPieces (lookup Right (fst (topLeftHardCornerLiftingCase (cube, moves) Right))))
  | getNth 8 (getPieces(lookup Down cube)) == getColorOfSide Top = fixRightTopCorners (topRightHardCornerLiftingCase (cube, moves) Right) (getPieces (lookup Right (fst (topRightHardCornerLiftingCase (cube, moves) Right))))
  | otherwise                                                    = (cube, moves)
 
fixLeftTopCorners :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixLeftTopCorners (cube, moves) pieces 
  | getNth 0 pieces                        == getColorOfSide Top = fixLeftTopCorners (leftCornerLiftingCase (skillfulTwist0 (cube, moves) Left) Left) (getPieces (lookup Left (fst (leftCornerLiftingCase (skillfulTwist0 (cube, moves) Left) Left))))
  | getNth 2 pieces                        == getColorOfSide Top = fixLeftTopCorners (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Left) Left) (getPieces (lookup Left (fst (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Left) Left))))
  | getNth 6 pieces                        == getColorOfSide Top = fixLeftTopCorners (leftCornerLiftingCase (cube, moves) Left) (getPieces (lookup Left (fst (leftCornerLiftingCase (cube, moves) Left))))
  | getNth 8 pieces                        == getColorOfSide Top = fixLeftTopCorners (rightCornerLiftingCase (cube, moves) Left) (getPieces (lookup Left (fst (rightCornerLiftingCase (cube, moves) Left))))
  | getNth 6 (getPieces(lookup Down cube)) == getColorOfSide Top = fixLeftTopCorners (topLeftHardCornerLiftingCase (cube, moves) Left) (getPieces (lookup Left (fst (topLeftHardCornerLiftingCase (cube, moves) Left))))
  | getNth 0 (getPieces(lookup Down cube)) == getColorOfSide Top = fixLeftTopCorners (topRightHardCornerLiftingCase (cube, moves) Left) (getPieces (lookup Left (fst (topRightHardCornerLiftingCase (cube, moves) Left))))
  | otherwise                                                    = (cube, moves)

fixBackTopCorners :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixBackTopCorners (cube, moves) pieces 
  | getNth 0 pieces                        == getColorOfSide Top = fixBackTopCorners (leftCornerLiftingCase (skillfulTwist0 (cube, moves) Back) Back) (getPieces (lookup Back (fst (leftCornerLiftingCase (skillfulTwist0 (cube, moves) Back) Back))))
  | getNth 2 pieces                        == getColorOfSide Top = fixBackTopCorners (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Back) Back) (getPieces (lookup Back (fst (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Back) Back))))
  | getNth 6 pieces                        == getColorOfSide Top = fixBackTopCorners (leftCornerLiftingCase (cube, moves) Back) (getPieces (lookup Back (fst (leftCornerLiftingCase (cube, moves) Back))))
  | getNth 8 pieces                        == getColorOfSide Top = fixBackTopCorners (rightCornerLiftingCase (cube, moves) Back) (getPieces (lookup Back (fst (rightCornerLiftingCase (cube, moves) Back))))
  | getNth 8 (getPieces(lookup Down cube)) == getColorOfSide Top = fixBackTopCorners (topLeftHardCornerLiftingCase (cube, moves) Back) (getPieces (lookup Back (fst (topLeftHardCornerLiftingCase (cube, moves) Back))))
  | getNth 6 (getPieces(lookup Down cube)) == getColorOfSide Top = fixBackTopCorners (topRightHardCornerLiftingCase (cube, moves) Back) (getPieces (lookup Back (fst (topRightHardCornerLiftingCase (cube, moves) Back))))
  | otherwise                                                    = (cube, moves)

-- F', D', F, D
skillfulTwist0 :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
skillfulTwist0 (cube, moves) side =
  (translateMove (translateMove (translateMove (translateMove cube side 9) side 3) side 8) side 2, moves ++ [addMove side 9, addMove side 3, addMove side 8, addMove side 2])

-- F, D, F', D'
skillfulTwist2 :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
skillfulTwist2 (cube, moves) side =
  (translateMove (translateMove (translateMove (translateMove cube side 8) side 2) side 9) side 3, moves ++ [addMove side 8, addMove side 2, addMove side 9, addMove side 3])

-- F', D', D' F, D
hardSkillfulTwist0 :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
hardSkillfulTwist0 (cube, moves) side = 
  (translateMove (translateMove (translateMove (translateMove (translateMove cube side 9) side 3) side 3) side 8) side 2, moves ++ [addMove side 9, addMove side 3, addMove side 3, addMove side 8, addMove side 2])

-- F, D, D, F', D'
hardSkillfulTwist2 :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
hardSkillfulTwist2 (cube, moves) side = 
  (translateMove (translateMove (translateMove (translateMove (translateMove cube side 8) side 2) side 2) side 9) side 3, moves ++ [addMove side 8, addMove side 2, addMove side 2, addMove side 9, addMove side 3])

-- F', D' F
leftCornerLiftingCase :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
leftCornerLiftingCase (cube, moves) side = 
  let downPieces = getPieces (lookup Down cube) in
    case side of
      Front -> let leftPieces = getPieces (lookup Left cube) in
                 if (getNth 8 leftPieces == getColorOfSide Left) && (getNth 0 downPieces == getColorOfSide Front) then (translateMove (translateMove (translateMove cube side 9) side 3) side 8 , moves ++ [addMove side 9, addMove side 3, addMove side 8])
                 else (leftCornerLiftingCase (down cube, moves ++ [addMove side 2]) Right)
      Right -> let leftPieces = getPieces (lookup Front cube) in
                 if (getNth 8 leftPieces == getColorOfSide Front) && (getNth 2 downPieces == getColorOfSide Right) then (translateMove (translateMove (translateMove cube side 9) side 3) side 8 , moves ++ [addMove side 9, addMove side 3, addMove side 8])
                 else (leftCornerLiftingCase (down cube, moves ++ [addMove side 2]) Back)
      Back  -> let leftPieces = getPieces (lookup Right cube) in
                 if (getNth 8 leftPieces == getColorOfSide Right) && (getNth 8 downPieces == getColorOfSide Back) then (translateMove (translateMove (translateMove cube side 9) side 3) side 8 , moves ++ [addMove side 9, addMove side 3, addMove side 8])
                 else (leftCornerLiftingCase (down cube, moves ++ [addMove side 2]) Left)
      Left  -> let leftPieces = getPieces (lookup Back cube) in
                 if (getNth 8 leftPieces == getColorOfSide Back) && (getNth 6 downPieces == getColorOfSide Left) then (translateMove (translateMove (translateMove cube side 9) side 3) side 8 , moves ++ [addMove side 9, addMove side 3, addMove side 8])
                 else (leftCornerLiftingCase (down cube, moves ++ [addMove side 2]) Front)

-- F, D, F'
rightCornerLiftingCase :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
rightCornerLiftingCase (cube, moves) side = 
  let downPieces = getPieces (lookup Down cube) in
    case side of
      Front -> let rightPieces = getPieces (lookup Right cube) in
                 if (getNth 6 rightPieces == getColorOfSide Right) && (getNth 2 downPieces == getColorOfSide Front) then (translateMove (translateMove (translateMove cube side 8) side 2) side 9 , moves ++ [addMove side 8, addMove side 2, addMove side 9])
                 else (rightCornerLiftingCase (down cube, moves ++ [addMove side 2]) Right)
      Right -> let rightPieces = getPieces (lookup Back cube) in
                 if (getNth 6 rightPieces == getColorOfSide Back) && (getNth 8 downPieces == getColorOfSide Right) then (translateMove (translateMove (translateMove cube side 8) side 2) side 9 , moves ++ [addMove side 8, addMove side 2, addMove side 9])
                 else (rightCornerLiftingCase (down cube, moves ++ [addMove side 2]) Back)
      Back  -> let rightPieces = getPieces (lookup Left cube) in
                 if (getNth 6 rightPieces == getColorOfSide Left) && (getNth 6 downPieces == getColorOfSide Back) then (translateMove (translateMove (translateMove cube side 8) side 2) side 9 , moves ++ [addMove side 8, addMove side 2, addMove side 9])
                 else (rightCornerLiftingCase (down cube, moves ++ [addMove side 2]) Left)
      Left  -> let rightPieces = getPieces (lookup Front cube) in
                 if (getNth 6 rightPieces == getColorOfSide Front) && (getNth 0 downPieces == getColorOfSide Left) then (translateMove (translateMove (translateMove cube side 8) side 2) side 9 , moves ++ [addMove side 8, addMove side 2, addMove side 9])
                 else (rightCornerLiftingCase (down cube, moves ++ [addMove side 2]) Front)

-- F', D', D', F, D, F', D', F
topLeftHardCornerLiftingCase :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
topLeftHardCornerLiftingCase (cube, moves) side = 
  let frontPieces = getPieces (lookup side cube) in
    case side of
      Front -> let leftPieces = getPieces (lookup Left cube) in
                 if (getNth 8 leftPieces == getColorOfSide Front) && (getNth 6 frontPieces == getColorOfSide Left) then (translateMove (translateMove (translateMove (fst (hardSkillfulTwist0 (cube, moves) Front)) side 9) side 3) side 8, moves ++ [addMove side 9, addMove side 3, addMove side 3, addMove side 8, addMove side 2, addMove side 9, addMove side 3, addMove side 8])
                 else (topLeftHardCornerLiftingCase (down cube, moves ++ [addMove side 2]) Right)
      Right -> let leftPieces = getPieces (lookup Front cube) in
                 if (getNth 8 leftPieces == getColorOfSide Right) && (getNth 6 frontPieces == getColorOfSide Front) then (translateMove (translateMove (translateMove (fst (hardSkillfulTwist0 (cube, moves) Front)) side 9) side 3) side 8, moves ++ [addMove side 9, addMove side 3, addMove side 3, addMove side 8, addMove side 2, addMove side 9, addMove side 3, addMove side 8])
                 else (topLeftHardCornerLiftingCase (down cube, moves ++ [addMove side 2]) Back)
      Back -> let leftPieces = getPieces (lookup Right cube) in
                 if (getNth 8 leftPieces == getColorOfSide Back) && (getNth 6 frontPieces == getColorOfSide Right) then (translateMove (translateMove (translateMove (fst (hardSkillfulTwist0 (cube, moves) Front)) side 9) side 3) side 8, moves ++ [addMove side 9, addMove side 3, addMove side 3, addMove side 8, addMove side 2, addMove side 9, addMove side 3, addMove side 8])
                 else (topLeftHardCornerLiftingCase (down cube, moves ++ [addMove side 2]) Left)
      Left -> let leftPieces = getPieces (lookup Back cube) in
                 if (getNth 8 leftPieces == getColorOfSide Left) && (getNth 6 frontPieces == getColorOfSide Back) then (translateMove (translateMove (translateMove (fst (hardSkillfulTwist0 (cube, moves) Front)) side 9) side 3) side 8, moves ++ [addMove side 9, addMove side 3, addMove side 3, addMove side 8, addMove side 2, addMove side 9, addMove side 3, addMove side 8])
                 else (topLeftHardCornerLiftingCase (down cube, moves ++ [addMove side 2]) Front)

-- F, D, D, F', D', F, D, F'
topRightHardCornerLiftingCase :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
topRightHardCornerLiftingCase (cube, moves) side = 
  let frontPieces = getPieces (lookup side cube) in
    case side of
      Front -> let rightPieces = getPieces (lookup Right cube) in
                 if (getNth 6 rightPieces == getColorOfSide Front) && (getNth 8 frontPieces == getColorOfSide Right) then (translateMove (translateMove (translateMove (fst (hardSkillfulTwist2 (cube, moves) Front)) side 8) side 2) side 9, moves ++ [addMove side 8, addMove side 2, addMove side 2, addMove side 9, addMove side 3, addMove side 8, addMove side 2, addMove side 9])
                 else (topRightHardCornerLiftingCase (down cube, moves ++ [addMove side 2]) Right)
      Right -> let rightPieces = getPieces (lookup Back cube) in
                 if (getNth 6 rightPieces == getColorOfSide Right) && (getNth 8 frontPieces == getColorOfSide Back) then (translateMove (translateMove (translateMove (fst (hardSkillfulTwist2 (cube, moves) Front)) side 8) side 2) side 9, moves ++ [addMove side 8, addMove side 2, addMove side 2, addMove side 9, addMove side 3, addMove side 8, addMove side 2, addMove side 9])
                 else (topRightHardCornerLiftingCase (down cube, moves ++ [addMove side 2]) Back)
      Back -> let rightPieces = getPieces (lookup Left cube) in
                 if (getNth 6 rightPieces == getColorOfSide Back) && (getNth 8 frontPieces == getColorOfSide Left) then (translateMove (translateMove (translateMove (fst (hardSkillfulTwist2 (cube, moves) Front)) side 8) side 2) side 9, moves ++ [addMove side 8, addMove side 2, addMove side 2, addMove side 9, addMove side 3, addMove side 8, addMove side 2, addMove side 9])
                 else (topRightHardCornerLiftingCase (down cube, moves ++ [addMove side 2]) Left)
      Left -> let rightPieces = getPieces (lookup Front cube) in
                 if (getNth 6 rightPieces == getColorOfSide Left) && (getNth 8 frontPieces == getColorOfSide Front) then (translateMove (translateMove (translateMove (fst (hardSkillfulTwist2 (cube, moves) Front)) side 8) side 2) side 9, moves ++ [addMove side 8, addMove side 2, addMove side 2, addMove side 9, addMove side 3, addMove side 8, addMove side 2, addMove side 9])
                 else (topRightHardCornerLiftingCase (down cube, moves ++ [addMove side 2]) Front)

-- Stage 3

solveMiddleLayer :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
solveMiddleLayer (cube, moves) = case checkMiddleLayer cube of
  True  -> (cube, moves)
  False -> fixMiddleLayer (cube, moves)

checkMiddleLayer :: VJRubiksCube -> Bool
checkMiddleLayer cube = 
  let frontPieces = getPieces (lookup Front cube) in
    let rightPieces = getPieces (lookup Right cube) in
      let leftPieces = getPieces (lookup Left cube) in
        let backPieces = getPieces (lookup Back cube) in
          if ((getNth 3 frontPieces == getColorOfSide Front) &&
              (getNth 5 frontPieces == getColorOfSide Front) &&
              (getNth 3 rightPieces == getColorOfSide Right) &&
              (getNth 5 rightPieces == getColorOfSide Right) &&
              (getNth 3 leftPieces  == getColorOfSide Left)  &&
              (getNth 5 leftPieces  == getColorOfSide Left)  &&
              (getNth 3 backPieces  == getColorOfSide Back)  &&
              (getNth 5 backPieces  == getColorOfSide Back)) then True else False

fixMiddleLayer :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
fixMiddleLayer (cube, moves) = 
  let (cube', moves') = fixFrontMiddleLayer (cube, moves) (getPieces (lookup Front cube)) in
    let (cube'', moves'') = fixRightMiddleLayer (cube', moves') (getPieces (lookup Right cube')) in
      let (cube''', moves''') = fixLeftMiddleLayer (cube'', moves'') (getPieces (lookup Left cube'')) in
        fixBackMiddleLayer (cube''', moves''') (getPieces (lookup Back cube'''))

fixFrontMiddleLayer :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixFrontMiddleLayer (cube, moves) pieces 
  | getNth 7 pieces                        /= getColorOfSide Bottom && (getNth 1 (getPieces(lookup Bottom cube)) /= getColorOfSide Bottom) = fixFrontMiddleLayer (fixMiddleLayerOrientation cube Front) (getPieces (lookup Front (fst (fixMiddleLayerOrientation cube Front))))
  -- | getNth 2 pieces                        == getColorOfSide Top = fixFrontTopCorners (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Front) Front) (getPieces (lookup Front (fst (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Front) Front))))
  -- | getNth 6 pieces                        == getColorOfSide Top = fixFrontTopCorners (leftCornerLiftingCase (cube, moves) Front) (getPieces (lookup Front (fst (leftCornerLiftingCase (cube, moves) Front))))
  -- | getNth 8 pieces                        == getColorOfSide Top = fixFrontTopCorners (rightCornerLiftingCase (cube, moves) Front) (getPieces (lookup Front (fst (rightCornerLiftingCase (cube, moves) Front))))
  -- | getNth 0 (getPieces(lookup Down cube)) == getColorOfSide Top = fixFrontTopCorners (topLeftHardCornerLiftingCase (cube, moves) Front) (getPieces (lookup Front (fst (topLeftHardCornerLiftingCase (cube, moves) Front))))
  -- | getNth 2 (getPieces(lookup Down cube)) == getColorOfSide Top = fixFrontTopCorners (topRightHardCornerLiftingCase (cube, moves) Front) (getPieces (lookup Front (fst (topRightHardCornerLiftingCase (cube, moves) Front))))
  -- | otherwise                                                    = (cube, moves)

fixMiddleLayerOrientation :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move]) 
fixMiddleLayerOrientation (cube, moves) side = 
  let frontPieces = getPieces (lookup side cube) in 
    let downPieces = getPieces (lookup Bottom cube) in 
      case side of
        Front -> case getNth 7 frontPieces of
          getColorOfSide Front -> if (getNth 1 downPieces == getColorOfSide Left) then RightAlgorithm (cube, moves)
                                  else LeftAlgorithm (cube,moves)
          _ -> fixMiddleLayerOrientation (down cube, moves ++ [addMove side 2]) Right 
        Right -> case getNth 7 frontPieces of
          getColorOfSide Front -> if (getNth 5 downPieces == getColorOfSide Left) then RightAlgorithm (cube, moves)
                                  else LeftAlgorithm (cube, moves) 
          _ -> fixMiddleLayerOrientation (down cube, moves ++ [addMove side 2]) Right 
        Back -> case getNth 7 frontPieces of
          getColorOfSide Front -> if (getNth 7 downPieces == getColorOfSide Left) then RightAlgorithm (cube, moves)
                                  else LeftAlgorithm (cube, moves) 
          _ -> fixMiddleLayerOrientation (down cube, moves ++ [addMove side 2]) Right 
        Left -> case getNth 7 frontPieces of
          getColorOfSide Front -> if (getNth 3 downPieces == getColorOfSide Left) then RightAlgorithm (cube, moves)
                                  else LeftAlgorithm (cube, moves) 
          _ -> fixMiddleLayerOrientation (down cube, moves ++ [addMove side 2]) Right 
        
RightAlgorithm :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move]) 
RightAlgorithm (cube, moves) =  
  (translateMove (translateMove (translateMove (translateMove (translateMove (translateMove (translateMove (translateMove cube Front Down 0) Front Down 6) Front Down 1) Front Down 7) Front Down 1) Front Down 9) Front Down 0) Front Down 8, moves ++ (map (addMove Front Down) [0,6,1,7,1,9,0,8]))         

LeftAlgorithm :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
LeftAlgorithm (cube, moves) = 
  (translateMove (translateMove (translateMove (translateMove (translateMove (translateMove (translateMove (translateMove cube Front Down 1) Front Down 5) Front Down 0) Front Down 4) Front Down 0) Front Down 8) Front Down 1) Front Down 9, moves ++ (map (addMove Front Down) [1,5,0,4,0,8,1,9]))
--               