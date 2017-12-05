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
  deriving (Eq, Show, Ord)

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

getColorOfSide :: Side -> Color
getColorOfSide side = case side of
  Up   -> Yellow
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
      replaceNth d3 (src!!s3) a'   

getPieces :: Maybe [Piece] -> [Piece]
getPieces pieces =
  case pieces of
    Nothing -> []
    Just p  -> p

up :: VJRubiksCube -> VJRubiksCube
up cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Up cube) in
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
    [(Front, f''), (Back, b''), (Left, l''), (Right, r''), (Up, t''''), (Down, d)]

up' :: VJRubiksCube -> VJRubiksCube
up' cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Up cube) in
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
    [(Front, f''), (Back, b''), (Left, l''), (Right, r''), (Up, t''''), (Down, d)]

down :: VJRubiksCube -> VJRubiksCube
down cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Up cube) in
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
    [(Front, f''), (Back, b''), (Left, l''), (Right, r''), (Up, t), (Down, d'''')]

down' :: VJRubiksCube -> VJRubiksCube
down' cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Up cube) in
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
    [(Front, f''), (Back, b''), (Left, l''), (Right, r''), (Up, t), (Down, d'''')]

left :: VJRubiksCube -> VJRubiksCube
left cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Up cube) in
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
    [(Front, f''), (Back, b''), (Left, l''''), (Right, r), (Up, t''), (Down, d'')]

left' :: VJRubiksCube -> VJRubiksCube
left' cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Up cube) in
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
    [(Front, f''), (Back, b''), (Left, l''''), (Right, r), (Up, t''), (Down, d'')]

right :: VJRubiksCube -> VJRubiksCube
right cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Up cube) in
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
    [(Front, f''), (Back, b''), (Left, l), (Right, r''''), (Up, t''), (Down, d'')]

right' :: VJRubiksCube -> VJRubiksCube
right' cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Up cube) in
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
    [(Front, f''), (Back, b''), (Left, l), (Right, r''''), (Up, t''), (Down, d'')]

front :: VJRubiksCube -> VJRubiksCube
front cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Up cube) in
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
    [(Front, f''''), (Back, b), (Left, l''), (Right, r''), (Up, t''), (Down, d'')]

front' :: VJRubiksCube -> VJRubiksCube
front' cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Up cube) in
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
    [(Front, f''''), (Back, b), (Left, l''), (Right, r''), (Up, t''), (Down, d'')]      

back :: VJRubiksCube -> VJRubiksCube
back cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Up cube) in
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
    [(Front, f), (Back, b''''), (Left, l''), (Right, r''), (Up, t''), (Down, d'')]

back' :: VJRubiksCube -> VJRubiksCube
back' cube = 
  let b = getPieces (lookup Back cube) in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Up cube) in
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
    [(Front, f), (Back, b''''), (Left, l''), (Right, r''), (Up, t''), (Down, d'')]      

solvedCube :: VJRubiksCube
solvedCube = 
  [(Down,[White,White,White,White,White,White,White,White,White]),
  (Up,[Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow]),
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
       
translateMove :: VJRubiksCube -> Side -> Side -> Int -> VJRubiksCube
translateMove cube frontSide upSide move =
  case upSide of
    Up -> case frontSide of
      Front -> case move of
        0   -> doMove 0 cube
        1   -> doMove 1 cube
        2   -> doMove 2 cube
        3   -> doMove 3 cube
        4   -> doMove 4 cube
        5   -> doMove 5 cube
        6   -> doMove 6 cube
        7   -> doMove 7 cube
        8   -> doMove 8 cube
        9   -> doMove 9 cube
        10  -> doMove 10 cube
        11  -> doMove 11 cube
      Right -> case move of
        0   -> doMove 0 cube
        1   -> doMove 1 cube
        2   -> doMove 2 cube
        3   -> doMove 3 cube
        4   -> doMove 8 cube
        5   -> doMove 9 cube
        6   -> doMove 10 cube
        7   -> doMove 11 cube
        8   -> doMove 6 cube
        9   -> doMove 7 cube
        10  -> doMove 4 cube
        11  -> doMove 5 cube
      Left  -> case move of 
        0   -> doMove 0 cube
        1   -> doMove 1 cube
        2   -> doMove 2 cube
        3   -> doMove 3 cube
        4   -> doMove 10 cube
        5   -> doMove 11 cube
        6   -> doMove 8 cube
        7   -> doMove 9 cube
        8   -> doMove 4 cube
        9   -> doMove 5 cube
        10  -> doMove 6 cube
        11  -> doMove 7 cube  
      Back  -> case move of
        0   -> doMove 0 cube
        1   -> doMove 1 cube
        2   -> doMove 2 cube
        3   -> doMove 3 cube
        4   -> doMove 6 cube
        5   -> doMove 7 cube
        6   -> doMove 4 cube
        7   -> doMove 5 cube
        8   -> doMove 10 cube
        9   -> doMove 11 cube
        10  -> doMove 8 cube
        11  -> doMove 9 cube
    Down -> case frontSide of 
      Front -> case move of
        0   -> doMove 2 cube
        1   -> doMove 3 cube
        2   -> doMove 0 cube
        3   -> doMove 1 cube
        4   -> doMove 6 cube
        5   -> doMove 7 cube
        6   -> doMove 4 cube
        7   -> doMove 5 cube
        8   -> doMove 8 cube
        9   -> doMove 9 cube
        10  -> doMove 10 cube
        11  -> doMove 11 cube
      Right -> case move of
        0   -> doMove 2 cube
        1   -> doMove 3 cube
        2   -> doMove 0 cube
        3   -> doMove 1 cube
        4   -> doMove 8 cube
        5   -> doMove 9 cube
        6   -> doMove 10 cube
        7   -> doMove 11 cube
        8   -> doMove 4 cube
        9   -> doMove 5 cube
        10  -> doMove 6 cube
        11  -> doMove 7 cube
      Left  -> case move of 
        0   -> doMove 2 cube
        1   -> doMove 3 cube
        2   -> doMove 0 cube
        3   -> doMove 1 cube
        4   -> doMove 10 cube
        5   -> doMove 11 cube
        6   -> doMove 8 cube
        7   -> doMove 9 cube
        8   -> doMove 6 cube
        9   -> doMove 7 cube
        10  -> doMove 4 cube
        11  -> doMove 5 cube  
      Back  -> case move of
        0   -> doMove 2 cube
        1   -> doMove 3 cube
        2   -> doMove 0 cube
        3   -> doMove 1 cube
        4   -> doMove 4 cube
        5   -> doMove 5 cube
        6   -> doMove 6 cube
        7   -> doMove 7 cube
        8   -> doMove 10 cube
        9   -> doMove 11 cube
        10  -> doMove 8 cube
        11  -> doMove 9 cube    

addMove :: Side -> Side -> Int -> Move
addMove frontSide upSide move = 
  case upSide of
    Up -> case frontSide of
      Front -> case move of
        0   -> U
        1   -> U'
        2   -> D
        3   -> D'
        4   -> L
        5   -> L'
        6   -> R
        7   -> R'
        8   -> F
        9   -> F'
        10  -> B
        11  -> B'
      Right -> case move of
        0   -> U
        1   -> U'
        2   -> D
        3   -> D'
        4   -> F
        5   -> F'
        6   -> B
        7   -> B'
        8   -> R
        9   -> R'
        10  -> L
        11  -> L'
      Left  -> case move of 
        0   -> U
        1   -> U'
        2   -> D
        3   -> D'
        4   -> B
        5   -> B'
        6   -> F
        7   -> F'
        8   -> L
        9   -> L'
        10  -> R
        11  -> R'
      Back  -> case move of
        0   -> U
        1   -> U'
        2   -> D
        3   -> D'
        4   -> R
        5   -> R'
        6   -> L
        7   -> L'
        8   -> B
        9   -> B'
        10  -> F
        11  -> F'
    Down -> case frontSide of
      Front -> case move of
        0   -> D
        1   -> D'
        2   -> U
        3   -> U'
        4   -> R
        5   -> R'
        6   -> L
        7   -> L'
        8   -> F
        9   -> F'
        10  -> B
        11  -> B'
      Right -> case move of
        0   -> D
        1   -> D'
        2   -> U
        3   -> U'
        4   -> F
        5   -> F'
        6   -> B
        7   -> B'
        8   -> L
        9   -> L'
        10  -> R
        11  -> R'
      Left  -> case move of 
        0   -> D
        1   -> D'
        2   -> U
        3   -> U'
        4   -> B
        5   -> B'
        6   -> F
        7   -> F'
        8   -> R
        9   -> R'
        10  -> L
        11  -> L'
      Back  -> case move of
        0   -> D
        1   -> D'
        2   -> U
        3   -> U'
        4   -> L
        5   -> L'
        6   -> R
        7   -> R'
        8   -> B
        9   -> B'
        10  -> F
        11  -> F'  

scramble :: VJRubiksCube -> IO VJRubiksCube
scramble cube = do
  seed <- newStdGen
  let list = randomlist 25 seed
  let alteredCube = foldl (\c n -> doMove n c) cube list
  return alteredCube

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . randomR(0, 11))


-- Stage 1: Extended Up Cross

solveUpCross :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
solveUpCross (cube, moves) =
  case checkUpExtendedCross cube of
    True  -> (cube, moves)
    False -> solveUpCross (fixUpEdges (cube, moves))

checkCross :: VJRubiksCube -> Side -> Bool
checkCross cube side = 
  let pieces = getPieces (lookup side cube) in
    if ((getNth 1 pieces == getColorOfSide side) &&
        (getNth 3 pieces == getColorOfSide side) &&
        (getNth 5 pieces == getColorOfSide side) &&
        (getNth 7 pieces == getColorOfSide side)) then True else False

checkUpExtendedCross :: VJRubiksCube -> Bool
checkUpExtendedCross cube = 
  let frontPieces = getPieces (lookup Front cube) in
    let rightPieces = getPieces (lookup Right cube) in
      let leftPieces = getPieces (lookup Left cube) in
        let backPieces = getPieces (lookup Back cube) in
          if ((getNth 1 frontPieces == getColorOfSide Front) &&
              (getNth 1 rightPieces == getColorOfSide Right) &&
              (getNth 1 leftPieces  == getColorOfSide Left)  &&
              (getNth 1 backPieces  == getColorOfSide Back)) &&
              (checkCross cube Up == True) then True else False

fixUpEdges :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
fixUpEdges (cube, moves) = 
  let (cube', moves') = fixFrontUpEdges (cube, moves) (getPieces (lookup Front cube)) in
    let (cube'', moves'') = fixRightUpEdges (cube', moves') (getPieces (lookup Right cube')) in
      let (cube''', moves''') = fixLeftUpEdges (cube'', moves'') (getPieces (lookup Left cube'')) in
        fixBackUpEdges (cube''', moves''') (getPieces (lookup Back cube'''))

fixFrontUpEdges :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixFrontUpEdges (cube, moves) pieces 
  | getNth 1 pieces                        == getColorOfSide Up = fixFrontUpEdges (frontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Front) Front) (getPieces (lookup Front (fst (frontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Front) Front))))
  | getNth 3 pieces                        == getColorOfSide Up = fixFrontUpEdges (downFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Front) Front) (getPieces (lookup Front (fst (downFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Front) Front))))
  | getNth 5 pieces                        == getColorOfSide Up = fixFrontUpEdges (downFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Front) Front) (getPieces (lookup Front (fst (downFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Front) Front))))
  | getNth 7 pieces                        == getColorOfSide Up = fixFrontUpEdges (frontFaceEdgeElevationCase (cube, moves) Front) (getPieces (lookup Front (fst (frontFaceEdgeElevationCase (cube, moves) Front))))
  | getNth 1 (getPieces(lookup Down cube)) == getColorOfSide Up = fixFrontUpEdges (downFaceEdgeElevationCase (cube, moves) Front) (getPieces (lookup Front (fst (downFaceEdgeElevationCase (cube, moves) Front))))
  | otherwise                                                   = (cube, moves)

fixRightUpEdges :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixRightUpEdges (cube, moves) pieces 
  | getNth 1 pieces                        == getColorOfSide Up = fixRightUpEdges (frontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Right) Right) (getPieces (lookup Right (fst (frontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Right) Right))))
  | getNth 3 pieces                        == getColorOfSide Up = fixRightUpEdges (downFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Right) Right) (getPieces (lookup Right (fst (downFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Right) Right))))
  | getNth 5 pieces                        == getColorOfSide Up = fixRightUpEdges (downFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Right) Right) (getPieces (lookup Right (fst (downFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Right) Right))))
  | getNth 7 pieces                        == getColorOfSide Up = fixRightUpEdges (frontFaceEdgeElevationCase (cube, moves) Right) (getPieces (lookup Right (fst (frontFaceEdgeElevationCase (cube, moves) Right))))
  | getNth 5 (getPieces(lookup Down cube)) == getColorOfSide Up = fixRightUpEdges (downFaceEdgeElevationCase (cube, moves) Right) (getPieces (lookup Right (fst (downFaceEdgeElevationCase (cube, moves) Right))))
  | otherwise                                                   = (cube, moves)
 
fixLeftUpEdges :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixLeftUpEdges (cube, moves) pieces 
  | getNth 1 pieces                        == getColorOfSide Up = fixLeftUpEdges (frontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Left) Left) (getPieces (lookup Left (fst (frontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Left) Left))))
  | getNth 3 pieces                        == getColorOfSide Up = fixLeftUpEdges (downFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Left) Left) (getPieces (lookup Left (fst (downFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Left) Left))))
  | getNth 5 pieces                        == getColorOfSide Up = fixLeftUpEdges (downFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Left) Left) (getPieces (lookup Left (fst (downFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Left) Left))))
  | getNth 7 pieces                        == getColorOfSide Up = fixLeftUpEdges (frontFaceEdgeElevationCase (cube, moves) Left) (getPieces (lookup Left (fst (frontFaceEdgeElevationCase (cube, moves) Left))))
  | getNth 3 (getPieces(lookup Down cube)) == getColorOfSide Up = fixLeftUpEdges (downFaceEdgeElevationCase (cube, moves) Left) (getPieces (lookup Left (fst (downFaceEdgeElevationCase (cube, moves) Left))))
  | otherwise                                                   = (cube, moves)

fixBackUpEdges :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move]) 
fixBackUpEdges (cube, moves) pieces  
  | getNth 1 pieces                        == getColorOfSide Up = fixBackUpEdges (frontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Back) Back) (getPieces (lookup Back (fst (frontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Back) Back))))
  | getNth 3 pieces                        == getColorOfSide Up = fixBackUpEdges (downFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Back) Back) (getPieces (lookup Back (fst (downFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Back) Back))))
  | getNth 5 pieces                        == getColorOfSide Up = fixBackUpEdges (downFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Back) Back) (getPieces (lookup Back (fst (downFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Back) Back))))
  | getNth 7 pieces                        == getColorOfSide Up = fixBackUpEdges (frontFaceEdgeElevationCase (cube, moves) Back) (getPieces (lookup Back (fst (frontFaceEdgeElevationCase (cube, moves) Back))))
  | getNth 7 (getPieces(lookup Down cube)) == getColorOfSide Up = fixBackUpEdges (downFaceEdgeElevationCase (cube, moves) Back) (getPieces (lookup Back (fst (downFaceEdgeElevationCase (cube, moves) Back))))
  | otherwise                                                   = (cube, moves)

-- F, F
skillfulTwist1 :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
skillfulTwist1 (cube, moves) side =
  (foldl (\a x -> translateMove a side Up x) cube [8,8], moves ++ (map (addMove side Up) [8,8]))
  
-- L, D, L'
skillfulTwist3 :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
skillfulTwist3 (cube, moves) side =
  (foldl (\a x -> translateMove a side Up x) cube [4,2,5], moves ++ (map (addMove side Up) [4,2,5]))

-- R', D', R
skillfulTwist5 :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
skillfulTwist5 (cube, moves) side =
  (foldl (\a x -> translateMove a side Up x) cube [7,3,6], moves ++ (map (addMove side Up) [7,3,6]))

-- D, R, F', R'
frontFaceEdgeElevationCase :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
frontFaceEdgeElevationCase (cube, moves) side = 
  let pieces = getPieces (lookup Down cube) in
    case side of
      Front -> if (getNth 1 pieces == getColorOfSide side) then (foldl (\a x -> translateMove a side Up x) cube [2,6,9,7], moves ++ (map (addMove side Up) [2,6,9,7]))
               else (frontFaceEdgeElevationCase (down cube, moves ++ [addMove side Up 2]) Right)
      Right -> if (getNth 5 pieces == getColorOfSide side) then (foldl (\a x -> translateMove a side Up x) cube [2,6,9,7], moves ++ (map (addMove side Up) [2,6,9,7]))
               else (frontFaceEdgeElevationCase (down cube, moves ++ [addMove side Up 2]) Back)
      Back  -> if (getNth 7 pieces == getColorOfSide side) then (foldl (\a x -> translateMove a side Up x) cube [2,6,9,7], moves ++ (map (addMove side Up) [2,6,9,7]))
               else (frontFaceEdgeElevationCase (down cube, moves ++ [addMove side Up 2]) Left)
      Left  -> if (getNth 3 pieces == getColorOfSide side) then (foldl (\a x -> translateMove a side Up x) cube [2,6,9,7], moves ++ (map (addMove side Up) [2,6,9,7]))
               else (frontFaceEdgeElevationCase (down cube, moves ++ [addMove side Up 2]) Front)

-- F, F
downFaceEdgeElevationCase :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
downFaceEdgeElevationCase (cube, moves) side = 
  let pieces = getPieces (lookup side cube) in
    case side of
      Front -> if (getNth 7 pieces == getColorOfSide side) then (foldl (\a x -> translateMove a side Up x) cube [8,8], moves ++ (map (addMove side Up) [8,8]))
               else (downFaceEdgeElevationCase (down cube, moves ++ [addMove side Up 2]) Right)
      Right -> if (getNth 7 pieces == getColorOfSide side) then (foldl (\a x -> translateMove a side Up x) cube [8,8], moves ++ (map (addMove side Up) [8,8]))
               else (downFaceEdgeElevationCase (down cube, moves ++ [addMove side Up 2]) Back)
      Back  -> if (getNth 7 pieces == getColorOfSide side) then (foldl (\a x -> translateMove a side Up x) cube [8,8], moves ++ (map (addMove side Up) [8,8]))
               else (downFaceEdgeElevationCase (down cube, moves ++ [addMove side Up 2]) Left)
      Left  -> if (getNth 7 pieces == getColorOfSide side) then (foldl (\a x -> translateMove a side Up x) cube [8,8], moves ++ (map (addMove side Up) [8,8]))
               else (downFaceEdgeElevationCase (down cube, moves ++ [addMove side Up 2]) Front)


-- Stage 2: Extended Corners (Up Layer)

checkCorners :: VJRubiksCube -> Bool
checkCorners cube = 
  let upPieces = getPieces (lookup Up cube) in
    if ((getNth 0 upPieces == getColorOfSide Up) &&
        (getNth 2 upPieces == getColorOfSide Up) &&
        (getNth 6 upPieces == getColorOfSide Up) &&
        (getNth 8 upPieces == getColorOfSide Up)) then True else False

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

solveUpCorners :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
solveUpCorners (cube, moves) =
  case checkExtendedCorners cube of
    True  -> (cube, moves)
    False -> solveUpCorners (fixUpCorners (cube, moves))

fixUpCorners :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
fixUpCorners (cube, moves) = 
  let (cube', moves') = fixFrontUpCorners (cube, moves) (getPieces (lookup Front cube)) in
    let (cube'', moves'') = fixRightUpCorners (cube', moves') (getPieces (lookup Right cube')) in
      let (cube''', moves''') = fixLeftUpCorners (cube'', moves'') (getPieces (lookup Left cube'')) in
        fixBackUpCorners (cube''', moves''') (getPieces (lookup Back cube'''))

fixFrontUpCorners :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixFrontUpCorners (cube, moves) pieces 
  | getNth 0 pieces                        == getColorOfSide Up = fixFrontUpCorners (leftCornerLiftingCase (skillfulTwist0 (cube, moves) Front) Front) (getPieces (lookup Front (fst (leftCornerLiftingCase (skillfulTwist0 (cube, moves) Front) Front))))
  | getNth 2 pieces                        == getColorOfSide Up = fixFrontUpCorners (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Front) Front) (getPieces (lookup Front (fst (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Front) Front))))
  | getNth 6 pieces                        == getColorOfSide Up = fixFrontUpCorners (leftCornerLiftingCase (cube, moves) Front) (getPieces (lookup Front (fst (leftCornerLiftingCase (cube, moves) Front))))
  | getNth 8 pieces                        == getColorOfSide Up = fixFrontUpCorners (rightCornerLiftingCase (cube, moves) Front) (getPieces (lookup Front (fst (rightCornerLiftingCase (cube, moves) Front))))
  | getNth 0 (getPieces(lookup Down cube)) == getColorOfSide Up = fixFrontUpCorners (topLeftHardCornerLiftingCase (cube, moves) Front) (getPieces (lookup Front (fst (topLeftHardCornerLiftingCase (cube, moves) Front))))
  | getNth 2 (getPieces(lookup Down cube)) == getColorOfSide Up = fixFrontUpCorners (topRightHardCornerLiftingCase (cube, moves) Front) (getPieces (lookup Front (fst (topRightHardCornerLiftingCase (cube, moves) Front))))
  | otherwise                                                   = (cube, moves)

fixRightUpCorners :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixRightUpCorners (cube, moves) pieces 
  | getNth 0 pieces                        == getColorOfSide Up = fixRightUpCorners (leftCornerLiftingCase (skillfulTwist0 (cube, moves) Right) Right) (getPieces (lookup Right (fst (leftCornerLiftingCase (skillfulTwist0 (cube, moves) Right) Right))))
  | getNth 2 pieces                        == getColorOfSide Up = fixRightUpCorners (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Right) Right) (getPieces (lookup Right (fst (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Right) Right))))
  | getNth 6 pieces                        == getColorOfSide Up = fixRightUpCorners (leftCornerLiftingCase (cube, moves) Right) (getPieces (lookup Right (fst (leftCornerLiftingCase (cube, moves) Right))))
  | getNth 8 pieces                        == getColorOfSide Up = fixRightUpCorners (rightCornerLiftingCase (cube, moves) Right) (getPieces (lookup Right (fst (rightCornerLiftingCase (cube, moves) Right))))
  | getNth 2 (getPieces(lookup Down cube)) == getColorOfSide Up = fixRightUpCorners (topLeftHardCornerLiftingCase (cube, moves) Right) (getPieces (lookup Right (fst (topLeftHardCornerLiftingCase (cube, moves) Right))))
  | getNth 8 (getPieces(lookup Down cube)) == getColorOfSide Up = fixRightUpCorners (topRightHardCornerLiftingCase (cube, moves) Right) (getPieces (lookup Right (fst (topRightHardCornerLiftingCase (cube, moves) Right))))
  | otherwise                                                   = (cube, moves)
 
fixLeftUpCorners :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixLeftUpCorners (cube, moves) pieces 
  | getNth 0 pieces                        == getColorOfSide Up = fixLeftUpCorners (leftCornerLiftingCase (skillfulTwist0 (cube, moves) Left) Left) (getPieces (lookup Left (fst (leftCornerLiftingCase (skillfulTwist0 (cube, moves) Left) Left))))
  | getNth 2 pieces                        == getColorOfSide Up = fixLeftUpCorners (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Left) Left) (getPieces (lookup Left (fst (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Left) Left))))
  | getNth 6 pieces                        == getColorOfSide Up = fixLeftUpCorners (leftCornerLiftingCase (cube, moves) Left) (getPieces (lookup Left (fst (leftCornerLiftingCase (cube, moves) Left))))
  | getNth 8 pieces                        == getColorOfSide Up = fixLeftUpCorners (rightCornerLiftingCase (cube, moves) Left) (getPieces (lookup Left (fst (rightCornerLiftingCase (cube, moves) Left))))
  | getNth 6 (getPieces(lookup Down cube)) == getColorOfSide Up = fixLeftUpCorners (topLeftHardCornerLiftingCase (cube, moves) Left) (getPieces (lookup Left (fst (topLeftHardCornerLiftingCase (cube, moves) Left))))
  | getNth 0 (getPieces(lookup Down cube)) == getColorOfSide Up = fixLeftUpCorners (topRightHardCornerLiftingCase (cube, moves) Left) (getPieces (lookup Left (fst (topRightHardCornerLiftingCase (cube, moves) Left))))
  | otherwise                                                   = (cube, moves)

fixBackUpCorners :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixBackUpCorners (cube, moves) pieces 
  | getNth 0 pieces                        == getColorOfSide Up = fixBackUpCorners (leftCornerLiftingCase (skillfulTwist0 (cube, moves) Back) Back) (getPieces (lookup Back (fst (leftCornerLiftingCase (skillfulTwist0 (cube, moves) Back) Back))))
  | getNth 2 pieces                        == getColorOfSide Up = fixBackUpCorners (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Back) Back) (getPieces (lookup Back (fst (rightCornerLiftingCase (skillfulTwist2 (cube, moves) Back) Back))))
  | getNth 6 pieces                        == getColorOfSide Up = fixBackUpCorners (leftCornerLiftingCase (cube, moves) Back) (getPieces (lookup Back (fst (leftCornerLiftingCase (cube, moves) Back))))
  | getNth 8 pieces                        == getColorOfSide Up = fixBackUpCorners (rightCornerLiftingCase (cube, moves) Back) (getPieces (lookup Back (fst (rightCornerLiftingCase (cube, moves) Back))))
  | getNth 8 (getPieces(lookup Down cube)) == getColorOfSide Up = fixBackUpCorners (topLeftHardCornerLiftingCase (cube, moves) Back) (getPieces (lookup Back (fst (topLeftHardCornerLiftingCase (cube, moves) Back))))
  | getNth 6 (getPieces(lookup Down cube)) == getColorOfSide Up = fixBackUpCorners (topRightHardCornerLiftingCase (cube, moves) Back) (getPieces (lookup Back (fst (topRightHardCornerLiftingCase (cube, moves) Back))))
  | otherwise                                                   = (cube, moves)

-- F', D', F, D
skillfulTwist0 :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
skillfulTwist0 (cube, moves) side =
  (foldl (\a x -> translateMove a side Up x) cube [9,3,8,2], moves ++ (map (addMove side Up) [9,3,8,2]))

-- F, D, F', D'
skillfulTwist2 :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
skillfulTwist2 (cube, moves) side =
  (foldl (\a x -> translateMove a side Up x) cube [8,2,9,3], moves ++ (map (addMove side Up) [8,2,9,3]))

-- F', D', D' F, D
hardSkillfulTwist0 :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
hardSkillfulTwist0 (cube, moves) side = 
  (foldl (\a x -> translateMove a side Up x) cube [9,3,3,8,2], moves ++ (map (addMove side Up) [9,3,3,8,2]))

-- F, D, D, F', D'
hardSkillfulTwist2 :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
hardSkillfulTwist2 (cube, moves) side = 
  (foldl (\a x -> translateMove a side Up x) cube [8,2,2,9,3], moves ++ (map (addMove side Up) [8,2,2,9,3]))

-- F', D' F
leftCornerLiftingCase :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
leftCornerLiftingCase (cube, moves) side = 
  let downPieces = getPieces (lookup Down cube) in
    case side of
      Front -> let leftPieces = getPieces (lookup Left cube) in
                 if (getNth 8 leftPieces == getColorOfSide Left) && (getNth 0 downPieces == getColorOfSide Front) then (foldl (\a x -> translateMove a side Up x) cube [9,3,8], moves ++ (map (addMove side Up) [9,3,8]))
                 else (leftCornerLiftingCase (down cube, moves ++ [addMove side Up 2]) Right)
      Right -> let leftPieces = getPieces (lookup Front cube) in
                 if (getNth 8 leftPieces == getColorOfSide Front) && (getNth 2 downPieces == getColorOfSide Right) then (foldl (\a x -> translateMove a side Up x) cube [9,3,8], moves ++ (map (addMove side Up) [9,3,8]))
                 else (leftCornerLiftingCase (down cube, moves ++ [addMove side Up 2]) Back)
      Back  -> let leftPieces = getPieces (lookup Right cube) in
                 if (getNth 8 leftPieces == getColorOfSide Right) && (getNth 8 downPieces == getColorOfSide Back) then (foldl (\a x -> translateMove a side Up x) cube [9,3,8], moves ++ (map (addMove side Up) [9,3,8]))
                 else (leftCornerLiftingCase (down cube, moves ++ [addMove side Up 2]) Left)
      Left  -> let leftPieces = getPieces (lookup Back cube) in
                 if (getNth 8 leftPieces == getColorOfSide Back) && (getNth 6 downPieces == getColorOfSide Left) then (foldl (\a x -> translateMove a side Up x) cube [9,3,8], moves ++ (map (addMove side Up) [9,3,8]))
                 else (leftCornerLiftingCase (down cube, moves ++ [addMove side Up 2]) Front)

-- F, D, F'
rightCornerLiftingCase :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
rightCornerLiftingCase (cube, moves) side = 
  let downPieces = getPieces (lookup Down cube) in
    case side of
      Front -> let rightPieces = getPieces (lookup Right cube) in
                 if (getNth 6 rightPieces == getColorOfSide Right) && (getNth 2 downPieces == getColorOfSide Front) then (foldl (\a x -> translateMove a side Up x) cube [8,2,9], moves ++ (map (addMove side Up) [8,2,9]))
                 else (rightCornerLiftingCase (down cube, moves ++ [addMove side Up 2]) Right)
      Right -> let rightPieces = getPieces (lookup Back cube) in
                 if (getNth 6 rightPieces == getColorOfSide Back) && (getNth 8 downPieces == getColorOfSide Right) then (foldl (\a x -> translateMove a side Up x) cube [8,2,9], moves ++ (map (addMove side Up) [8,2,9]))
                 else (rightCornerLiftingCase (down cube, moves ++ [addMove side Up 2]) Back)
      Back  -> let rightPieces = getPieces (lookup Left cube) in
                 if (getNth 6 rightPieces == getColorOfSide Left) && (getNth 6 downPieces == getColorOfSide Back) then (foldl (\a x -> translateMove a side Up x) cube [8,2,9], moves ++ (map (addMove side Up) [8,2,9]))
                 else (rightCornerLiftingCase (down cube, moves ++ [addMove side Up 2]) Left)
      Left  -> let rightPieces = getPieces (lookup Front cube) in
                 if (getNth 6 rightPieces == getColorOfSide Front) && (getNth 0 downPieces == getColorOfSide Left) then (foldl (\a x -> translateMove a side Up x) cube [8,2,9], moves ++ (map (addMove side Up) [8,2,9]))
                 else (rightCornerLiftingCase (down cube, moves ++ [addMove side Up 2]) Front)

-- F', D', D', F, D, F', D', F
topLeftHardCornerLiftingCase :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
topLeftHardCornerLiftingCase (cube, moves) side = 
  let frontPieces = getPieces (lookup side cube) in
    case side of
      Front -> let leftPieces = getPieces (lookup Left cube) in
                 if (getNth 8 leftPieces == getColorOfSide Front) && (getNth 6 frontPieces == getColorOfSide Left) then (foldl (\a x -> translateMove a side Up x) (fst (hardSkillfulTwist0 (cube, moves) Front)) [9,3,8], moves ++ (map (addMove side Up) [9,3,8]))
                 else (topLeftHardCornerLiftingCase (down cube, moves ++ [addMove side Up 2]) Right)
      Right -> let leftPieces = getPieces (lookup Front cube) in
                 if (getNth 8 leftPieces == getColorOfSide Right) && (getNth 6 frontPieces == getColorOfSide Front) then (foldl (\a x -> translateMove a side Up x) (fst (hardSkillfulTwist0 (cube, moves) Right)) [9,3,8], moves ++ (map (addMove side Up) [9,3,8]))
                 else (topLeftHardCornerLiftingCase (down cube, moves ++ [addMove side Up 2]) Back)
      Back  -> let leftPieces = getPieces (lookup Right cube) in
                 if (getNth 8 leftPieces == getColorOfSide Back) && (getNth 6 frontPieces == getColorOfSide Right) then (foldl (\a x -> translateMove a side Up x) (fst (hardSkillfulTwist0 (cube, moves) Back)) [9,3,8], moves ++ (map (addMove side Up) [9,3,8]))
                 else (topLeftHardCornerLiftingCase (down cube, moves ++ [addMove side Up 2]) Left)
      Left  -> let leftPieces = getPieces (lookup Back cube) in
                 if (getNth 8 leftPieces == getColorOfSide Left) && (getNth 6 frontPieces == getColorOfSide Back) then (foldl (\a x -> translateMove a side Up x) (fst (hardSkillfulTwist0 (cube, moves) Left)) [9,3,8], moves ++ (map (addMove side Up) [9,3,8]))
                 else (topLeftHardCornerLiftingCase (down cube, moves ++ [addMove side Up 2]) Front)

-- F, D, D, F', D', F, D, F'
topRightHardCornerLiftingCase :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
topRightHardCornerLiftingCase (cube, moves) side = 
  let frontPieces = getPieces (lookup side cube) in
    case side of
      Front -> let rightPieces = getPieces (lookup Right cube) in
                 if (getNth 6 rightPieces == getColorOfSide Front) && (getNth 8 frontPieces == getColorOfSide Right) then (foldl (\a x -> translateMove a side Up x) (fst (hardSkillfulTwist2 (cube, moves) Front)) [8,2,9], moves ++ (map (addMove side Up) [8,2,9]))
                 else (topRightHardCornerLiftingCase (down cube, moves ++ [addMove side Up 2]) Right)
      Right -> let rightPieces = getPieces (lookup Back cube) in
                 if (getNth 6 rightPieces == getColorOfSide Right) && (getNth 8 frontPieces == getColorOfSide Back) then (foldl (\a x -> translateMove a side Up x) (fst (hardSkillfulTwist2 (cube, moves) Right)) [8,2,9], moves ++ (map (addMove side Up) [8,2,9]))
                 else (topRightHardCornerLiftingCase (down cube, moves ++ [addMove side Up 2]) Back)
      Back  -> let rightPieces = getPieces (lookup Left cube) in
                 if (getNth 6 rightPieces == getColorOfSide Back) && (getNth 8 frontPieces == getColorOfSide Left) then (foldl (\a x -> translateMove a side Up x) (fst (hardSkillfulTwist2 (cube, moves) Back)) [8,2,9], moves ++ (map (addMove side Up) [8,2,9]))
                 else (topRightHardCornerLiftingCase (down cube, moves ++ [addMove side Up 2]) Left)
      Left  -> let rightPieces = getPieces (lookup Front cube) in
                 if (getNth 6 rightPieces == getColorOfSide Left) && (getNth 8 frontPieces == getColorOfSide Front) then (foldl (\a x -> translateMove a side Up x) (fst (hardSkillfulTwist2 (cube, moves) Left)) [8,2,9], moves ++ (map (addMove side Up) [8,2,9]))
                 else (topRightHardCornerLiftingCase (down cube, moves ++ [addMove side Up 2]) Front)


-- Stage 3: Middle Layer

solveMiddleLayer :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
solveMiddleLayer (cube, moves) =
  case checkMiddleLayer cube of
    True  -> (cube, moves)
    False -> solveMiddleLayer (fixMiddleLayer (cube, moves))

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
  | (getNth 7 pieces /= getColorOfSide Down &&
    (getNth 1 (getPieces(lookup Down cube)) /= getColorOfSide Down)) = fixMiddleLayerOrientation (cube, moves) Front
  | otherwise                                                        = (cube, moves)

fixRightMiddleLayer :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixRightMiddleLayer (cube, moves) pieces 
  | (getNth 7 pieces /= getColorOfSide Down &&
    (getNth 5 (getPieces(lookup Down cube)) /= getColorOfSide Down)) = fixMiddleLayerOrientation (cube, moves) Right
  | otherwise                                                        = (cube, moves)

fixBackMiddleLayer :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixBackMiddleLayer (cube, moves) pieces 
  | (getNth 7 pieces /= getColorOfSide Down &&
    (getNth 7 (getPieces(lookup Down cube)) /= getColorOfSide Down)) = fixMiddleLayerOrientation (cube, moves) Back
  | otherwise                                                        = (cube, moves)

fixLeftMiddleLayer :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixLeftMiddleLayer (cube, moves) pieces 
  | (getNth 7 pieces /= getColorOfSide Down &&
    (getNth 3 (getPieces(lookup Down cube)) /= getColorOfSide Down)) = fixMiddleLayerOrientation (cube, moves) Left
  | otherwise                                                        = (cube, moves)

fixMiddleLayerOrientation :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move]) 
fixMiddleLayerOrientation (cube, moves) side = 
  let frontPieces = getPieces (lookup side cube) in 
    let downPieces = getPieces (lookup Down cube) in 
      let sideColor = getColorOfSide side in
        case side of
          Front -> if (getNth 7 frontPieces == getColorOfSide side) && (getNth 1 downPieces == getColorOfSide Left) then rightAlgorithm (cube, moves) Front
                   else if (getNth 7 frontPieces == getColorOfSide side) && (getNth 1 downPieces == getColorOfSide Right) then leftAlgorithm (cube, moves) Front
                   else fixMiddleLayerOrientation (down cube, moves ++ [addMove side Up 2]) Right
          Right -> if (getNth 7 frontPieces == getColorOfSide side) && (getNth 5 downPieces == getColorOfSide Front) then rightAlgorithm (cube, moves) Right
                   else if (getNth 7 frontPieces == getColorOfSide side) && (getNth 5 downPieces == getColorOfSide Back) then leftAlgorithm (cube, moves) Right
                   else fixMiddleLayerOrientation (down cube, moves ++ [addMove side Up 2]) Back
          Back -> if (getNth 7 frontPieces == getColorOfSide side) && (getNth 7 downPieces == getColorOfSide Right) then rightAlgorithm (cube, moves) Back
                   else if (getNth 7 frontPieces == getColorOfSide side) && (getNth 7 downPieces == getColorOfSide Left) then leftAlgorithm (cube, moves) Back
                   else fixMiddleLayerOrientation (down cube, moves ++ [addMove side Up 2]) Left
          Left -> if (getNth 7 frontPieces == getColorOfSide side) && (getNth 3 downPieces == getColorOfSide Back) then rightAlgorithm (cube, moves) Left
                   else if (getNth 7 frontPieces == getColorOfSide side) && (getNth 3 downPieces == getColorOfSide Front) then leftAlgorithm (cube, moves) Left
                   else fixMiddleLayerOrientation (down cube, moves ++ [addMove side Up 2]) Front

-- U, R, U’, R’, U’, F’, U, F     
rightAlgorithm :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move]) 
rightAlgorithm (cube, moves) side =  
  (foldl (\a x -> translateMove a side Down x) cube [0,6,1,7,1,9,0,8], moves ++ (map (addMove side Down) [0,6,1,7,1,9,0,8]))

-- U’, L’, U, L, U, F, U’, F’
leftAlgorithm :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
leftAlgorithm (cube, moves) side = 
  (foldl (\a x -> translateMove a side Down x) cube [1,5,0,4,0,8,1,9], moves ++ (map (addMove side Down) [1,5,0,4,0,8,1,9]))


-- Stage 4: Down Cross

solveDownCross :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
solveDownCross (cube, moves) =
  case checkCross cube Down of
    True  -> (cube, moves)
    False -> solveDownCross (fixDownCross (cube, moves))

fixDownCross :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
fixDownCross (cube, moves)
  | checkHorizontalLineNS cube == True = permutationChangeAlgorithm (cube, moves) Left
  | checkHorizontalLineEW cube == True = permutationChangeAlgorithm (cube, moves) Front
  | checkLFront cube           == True = shortcutLToCrossAlgorithm (cube, moves) Front
  | checkLRight cube           == True = shortcutLToCrossAlgorithm (cube, moves) Left
  | checkLBack cube            == True = shortcutLToCrossAlgorithm (cube, moves) Back
  | checkLLeft cube            == True = shortcutLToCrossAlgorithm (cube, moves) Right
  | otherwise                          = permutationChangeAlgorithm (cube, moves) Front

checkHorizontalLineNS :: VJRubiksCube -> Bool
checkHorizontalLineNS cube
    | ((getNth 1 (getPieces (lookup Down cube)) == getColorOfSide Down) &&
       (getNth 4 (getPieces (lookup Down cube)) == getColorOfSide Down) &&
       (getNth 7 (getPieces (lookup Down cube)) == getColorOfSide Down))   = True
    | otherwise                                                            = False

checkHorizontalLineEW :: VJRubiksCube -> Bool
checkHorizontalLineEW cube
  | ((getNth 3 (getPieces (lookup Down cube)) == getColorOfSide Down) &&
     (getNth 4 (getPieces (lookup Down cube)) == getColorOfSide Down) &&
     (getNth 5 (getPieces (lookup Down cube)) == getColorOfSide Down))   = True
  | otherwise                                                            = False

checkLFront :: VJRubiksCube -> Bool
checkLFront cube
  | ((getNth 4 (getPieces (lookup Down cube)) == getColorOfSide Down) &&
     (getNth 5 (getPieces (lookup Down cube)) == getColorOfSide Down) &&
     (getNth 7 (getPieces (lookup Down cube)) == getColorOfSide Down))   = True
  | otherwise                                                            = False

checkLRight :: VJRubiksCube -> Bool
checkLRight cube
  | ((getNth 1 (getPieces (lookup Down cube)) == getColorOfSide Down) &&
     (getNth 4 (getPieces (lookup Down cube)) == getColorOfSide Down) &&
     (getNth 5 (getPieces (lookup Down cube)) == getColorOfSide Down))   = True
  | otherwise                                                            = False

checkLBack :: VJRubiksCube -> Bool
checkLBack cube
  | ((getNth 1 (getPieces (lookup Down cube)) == getColorOfSide Down) &&
     (getNth 3 (getPieces (lookup Down cube)) == getColorOfSide Down) &&
     (getNth 4 (getPieces (lookup Down cube)) == getColorOfSide Down))   = True
  | otherwise                                                            = False

checkLLeft :: VJRubiksCube -> Bool
checkLLeft cube
  | ((getNth 3 (getPieces (lookup Down cube)) == getColorOfSide Down) &&
     (getNth 4 (getPieces (lookup Down cube)) == getColorOfSide Down) &&
     (getNth 7 (getPieces (lookup Down cube)) == getColorOfSide Down))   = True
  | otherwise                                                            = False

-- F, R, U, R', U', F'
permutationChangeAlgorithm :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
permutationChangeAlgorithm (cube, moves) side = 
  (foldl (\a x -> translateMove a side Down x) cube [8,6,0,7,1,9], moves ++ (map (addMove side Down) [8,6,0,7,1,9]))

-- F, U, R, U', R', F'
shortcutLToCrossAlgorithm :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
shortcutLToCrossAlgorithm (cube, moves) side =
  (foldl (\a x -> translateMove a side Down x) cube [8,0,6,1,7,9], moves ++ (map (addMove side Down) [8,0,6,1,7,9]))


-- Stage 5: Extended Down Cross

solveDownExtendedCross :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
solveDownExtendedCross (cube, moves) =
  case checkDownExtendedCross cube of
    True  -> (cube, moves)
    False -> solveDownExtendedCross (fixDownExtendedCross (cube, moves))

checkDownExtendedCross :: VJRubiksCube -> Bool
checkDownExtendedCross cube = 
  let frontPieces = getPieces (lookup Front cube) in
    let rightPieces = getPieces (lookup Right cube) in
      let leftPieces = getPieces (lookup Left cube) in
        let backPieces = getPieces (lookup Back cube) in
          if ((getNth 7 frontPieces == getColorOfSide Front) &&
              (getNth 7 rightPieces == getColorOfSide Right) &&
              (getNth 7 leftPieces  == getColorOfSide Left)  &&
              (getNth 7 backPieces  == getColorOfSide Back)) &&
              (checkCross cube Down == True) then True else False

fixDownExtendedCross :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
fixDownExtendedCross (cube, moves) = 
  let frontPieces = getPieces (lookup Front cube) in
    let rightPieces = getPieces (lookup Right cube) in
      let leftPieces = getPieces (lookup Left cube) in
        let backPieces = getPieces (lookup Back cube) in
          if checkFBOppositeEdges cube == True then swapOppositeEdgesAlgorithm (cube, moves) Front
          else if checkLROppositeEdges cube == True then swapOppositeEdgesAlgorithm (cube, moves) Left
          else if checkFrontAdjacentEdge cube == True then swapClockwiseAdjacentEdgesAlgorithm (cube, moves) Front
          else if checkRightAdjacentEdge cube == True then swapClockwiseAdjacentEdgesAlgorithm (cube, moves) Right
          else if checkBackAdjacentEdge cube == True then swapClockwiseAdjacentEdgesAlgorithm (cube, moves) Back
          else if checkLeftAdjacentEdge cube == True then swapClockwiseAdjacentEdgesAlgorithm (cube, moves) Left
          else solveDownExtendedCross (down cube, moves ++ [addMove Down Up 2]) 

checkFBOppositeEdges :: VJRubiksCube -> Bool
checkFBOppositeEdges cube = 
  let frontPieces = getPieces (lookup Front cube) in
    let backPieces = getPieces (lookup Back cube) in
      if (getNth 7 frontPieces == getColorOfSide Back) && (getNth 7 backPieces == getColorOfSide Front) then True
      else False
  
checkLROppositeEdges :: VJRubiksCube -> Bool
checkLROppositeEdges cube = 
  let rightPieces = getPieces (lookup Right cube) in
    let leftPieces = getPieces (lookup Left cube) in
      if (getNth 7 rightPieces == getColorOfSide Left) && (getNth 7 leftPieces == getColorOfSide Right) then True
      else False

checkFrontAdjacentEdge :: VJRubiksCube -> Bool
checkFrontAdjacentEdge cube = 
  let frontPieces = getPieces (lookup Front cube) in
    let clockwisePieces = getPieces (lookup Right cube) in
      if (getNth 7 frontPieces == getColorOfSide Right) && (getNth 7 clockwisePieces == getColorOfSide Front) then True
      else False

checkRightAdjacentEdge :: VJRubiksCube -> Bool
checkRightAdjacentEdge cube = 
  let frontPieces = getPieces (lookup Left cube) in
    let clockwisePieces = getPieces (lookup Front cube) in
      if (getNth 7 frontPieces == getColorOfSide Front) && (getNth 7 clockwisePieces == getColorOfSide Left) then True
      else False

checkBackAdjacentEdge :: VJRubiksCube -> Bool
checkBackAdjacentEdge cube = 
  let frontPieces = getPieces (lookup Back cube) in
    let clockwisePieces = getPieces (lookup Left cube) in
      if (getNth 7 frontPieces == getColorOfSide Left) && (getNth 7 clockwisePieces == getColorOfSide Back) then True
      else False

checkLeftAdjacentEdge :: VJRubiksCube -> Bool
checkLeftAdjacentEdge cube = 
  let frontPieces = getPieces (lookup Right cube) in
    let clockwisePieces = getPieces (lookup Back cube) in
      if (getNth 7 frontPieces == getColorOfSide Back) && (getNth 7 clockwisePieces == getColorOfSide Right) then True
      else False

-- R, U, R', U, R, U, U, R', U
swapClockwiseAdjacentEdgesAlgorithm :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
swapClockwiseAdjacentEdgesAlgorithm (cube, moves) side = 
  (foldl (\a x -> translateMove a side Down x) cube [6,0,7,0,6,0,0,7,0], moves ++ (map (addMove side Down) [6,0,7,0,6,0,0,7,0]))

-- U, R, U, R', U, R, U, U, R', U, L, U, L', U, L, U, U, L', U
swapOppositeEdgesAlgorithm :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
swapOppositeEdgesAlgorithm (cube, moves) side = 
  (foldl (\a x -> translateMove a side Down x) cube [0,6,0,7,0,6,0,0,7,0,4,0,5,0,4,0,0,5,0], moves ++ (map (addMove side Down) [0,6,0,7,0,6,0,0,7,0,4,0,5,0,4,0,0,5,0]))


-- Stage 6  Swap Down Corners

solveDownCorners :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
solveDownCorners (cube, moves) = 
  case checkAllCorners cube of
    True  -> (cube, moves)
    False -> solveDownCorners (fixDownCorners (cube, moves))

checkAllCorners :: VJRubiksCube -> Bool
checkAllCorners cube = checkCornerFRU cube && checkCornerFLU cube && checkCornerRBU cube && checkCornerLBU cube

fixDownCorners :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
fixDownCorners (cube, moves) 
  | checkCornerFRU cube = swapCornerAlgorithm (cube, moves) Front
  | checkCornerFLU cube = swapCornerAlgorithm (cube, moves) Right
  | checkCornerRBU cube = swapCornerAlgorithm (cube, moves) Left
  | checkCornerLBU cube = swapCornerAlgorithm (cube, moves) Back
  | otherwise           = swapCornerAlgorithm (cube, moves) Front

checkCornerFRU :: VJRubiksCube -> Bool
checkCornerFRU cube = 
  let correctColors = map getColorOfSide [Front, Left, Down] in
    let currentColors = [getNth 6 (getPieces (lookup Front cube)),
                         getNth 8 (getPieces (lookup Left cube)),
                         getNth 0 (getPieces (lookup Down cube))] in
      sort correctColors == sort currentColors

checkCornerFLU :: VJRubiksCube -> Bool
checkCornerFLU cube = 
  let correctColors = map getColorOfSide [Front, Right, Down] in
    let currentColors = [getNth 8 (getPieces (lookup Front cube)),
                         getNth 6 (getPieces (lookup Right cube)),
                         getNth 2 (getPieces (lookup Down cube))] in
      sort correctColors == sort currentColors 

checkCornerRBU :: VJRubiksCube -> Bool
checkCornerRBU cube = 
  let correctColors = map getColorOfSide [Left, Back, Down] in
    let currentColors = [getNth 6 (getPieces (lookup Left cube)),
                         getNth 8 (getPieces (lookup Back cube)),
                         getNth 6 (getPieces (lookup Down cube))] in
      sort correctColors == sort currentColors 

checkCornerLBU :: VJRubiksCube -> Bool
checkCornerLBU cube = 
  let correctColors = map getColorOfSide [Right, Back, Down] in
    let currentColors = [getNth 8 (getPieces (lookup Right cube)),
                         getNth 6 (getPieces (lookup Back cube)),
                         getNth 8 (getPieces (lookup Down cube))] in
      sort correctColors == sort currentColors 

-- U, R, U', L', U, R', U', L
swapCornerAlgorithm :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
swapCornerAlgorithm (cube, moves) side = 
  (foldl (\a x -> translateMove a side Down x) cube [0,6,1,5,0,7,1,4], moves ++ (map (addMove side Down) [0,6,1,5,0,7,1,4]))


-- Stage 7: Orient Down Corners

solveFinalCorners :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
solveFinalCorners (cube, moves) = 
  case cube == solvedCube of
    True  -> (cube, moves)
    False -> fixFinalCorners (cube, moves)

findFinalFace :: VJRubiksCube -> Side
findFinalFace cube
  | checkCornerFRUStrict cube == False = Front
  | checkCornerFLUStrict cube == False = Right
  | checkCornerLBUStrict cube == False = Back
  | checkCornerRBUStrict cube == False = Left

fixFinalCorners :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
fixFinalCorners (cube, moves) = 
  case findFinalFace cube of
    Front -> fixFinalCornerFront (cube, moves) Front 0
    Right -> fixFinalCornerRight (cube, moves) Right 2
    Back  -> fixFinalCornerBack (cube, moves) Back 8
    Left  -> fixFinalCornerLeft (cube, moves) Left 6

fixFinalCornerFront :: (VJRubiksCube, [Move]) -> Side -> Int -> (VJRubiksCube, [Move])
fixFinalCornerFront (cube, moves) side index
  | getNth index (getPieces (lookup Down cube)) /= getColorOfSide Down = fixFinalCornerFront (orientCornerAlgorithm (cube, moves) side) side index
  | otherwise                                                          = fixFinalCornerRight (down cube, moves ++ [addMove side Up 2]) side index
 
fixFinalCornerRight :: (VJRubiksCube, [Move]) -> Side -> Int -> (VJRubiksCube, [Move])
fixFinalCornerRight (cube, moves) side index
  | getNth index (getPieces (lookup Down cube)) /= getColorOfSide Down = fixFinalCornerRight (orientCornerAlgorithm (cube, moves) side) side index
  | otherwise                                                          = fixFinalCornerBack (down cube, moves ++ [addMove side Up 2]) side index
 
fixFinalCornerBack :: (VJRubiksCube, [Move]) -> Side -> Int -> (VJRubiksCube, [Move])
fixFinalCornerBack (cube, moves) side index
  | getNth index (getPieces (lookup Down cube)) /= getColorOfSide Down = fixFinalCornerBack (orientCornerAlgorithm (cube, moves) side) side index
  | otherwise                                                          = fixFinalCornerLeft (down cube, moves ++ [addMove side Up 2]) side index

fixFinalCornerLeft :: (VJRubiksCube, [Move]) -> Side -> Int -> (VJRubiksCube, [Move])
fixFinalCornerLeft (cube, moves) side index
  | getNth index (getPieces (lookup Down cube)) /= getColorOfSide Down = fixFinalCornerLeft (orientCornerAlgorithm (cube, moves) side) side index
  | otherwise                                                          = (cube, moves)

-- True Front
checkCornerFRUStrict :: VJRubiksCube -> Bool
checkCornerFRUStrict cube = 
  let correctColors = map getColorOfSide [Front, Left, Down] in
    let currentColors = [getNth 6 (getPieces (lookup Front cube)),
                         getNth 8 (getPieces (lookup Left cube)),
                         getNth 0 (getPieces (lookup Down cube))] in
      correctColors == currentColors

-- True Right
checkCornerFLUStrict :: VJRubiksCube -> Bool
checkCornerFLUStrict cube = 
  let correctColors = map getColorOfSide [Front, Right, Down] in
    let currentColors = [getNth 8 (getPieces (lookup Front cube)),
                         getNth 6 (getPieces (lookup Right cube)),
                         getNth 2 (getPieces (lookup Down cube))] in
      correctColors == currentColors 

-- True Back
checkCornerLBUStrict :: VJRubiksCube -> Bool
checkCornerLBUStrict cube = 
  let correctColors = map getColorOfSide [Right, Back, Down] in
    let currentColors = [getNth 8 (getPieces (lookup Right cube)),
                         getNth 6 (getPieces (lookup Back cube)),
                         getNth 8 (getPieces (lookup Down cube))] in
      correctColors == currentColors

-- True Left
checkCornerRBUStrict :: VJRubiksCube -> Bool
checkCornerRBUStrict cube = 
  let correctColors = map getColorOfSide [Left, Back, Down] in
    let currentColors = [getNth 6 (getPieces (lookup Left cube)),
                         getNth 8 (getPieces (lookup Back cube)),
                         getNth 6 (getPieces (lookup Down cube))] in
      correctColors == currentColors 

-- R', D', R, D
orientCornerAlgorithm :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
orientCornerAlgorithm (cube, moves) side =
  (foldl (\a x -> translateMove a side Down x) cube [7,3,6,2], moves ++ (map (addMove side Down) [7,3,6,2]))


-- Stage 8: Finishing Touches

solveFinishingTouches :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
solveFinishingTouches (cube, moves) = 
  case cube == solvedCube of
    True  -> (cube, moves)
    False -> fixFinalTouches (cube, moves)

fixFinalTouches :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
fixFinalTouches (cube, moves) = 
  let frontPieces = getPieces (lookup Front cube) in
    if (getNth 1 frontPieces /= getColorOfSide Front) then fixFinalTouches (up cube, moves ++ [addMove Front Up 0])
    else if (getNth 7 frontPieces /= getColorOfSide Front) then fixFinalTouches (down cube, moves ++ [addMove Front Up 2])
    else (cube, moves)

solve :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
solve (cube, moves) =
  let r1 = solveUpCross (cube, moves) in
  let r2 = solveUpCorners r1 in
  let r3 = solveMiddleLayer r2 in
  let r4 = solveDownCross r3 in
  let r5 = solveDownExtendedCross r4 in
  let r6 = solveDownCorners r5 in
  let r7 = solveFinalCorners r6 in
    solveFinishingTouches r7

-- watDo :: (VJRubiksCube, [Move])
-- watDo =
--   let r = back (front (right (left solvedCube))) in
--   let r1 = solveUpCross (r, []) in
--   let r2 = solveUpCorners r1 in
--   let r3 = solveMiddleLayer r2 in
--   let r4 = solveDownCross r3 in
--   let r5 = solveDownExtendedCross r4 in
--   let r6 = solveDownCorners r5 in
--   let r7 = solveFinalCorners r6 in
--     solveFinishingTouches r7