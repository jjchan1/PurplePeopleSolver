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

scramble :: VJRubiksCube -> IO VJRubiksCube
scramble cube = do
  seed <- newStdGen
  let list = randomlist 25 seed
  let alteredCube = foldl (\c n -> doMove n c) cube list
  return alteredCube

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . randomR(0, 11))

solveYellowCross :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
solveYellowCross (cube, moves) = case checkExtendedCross cube of
  True -> (cube, [])
  False -> fixYellowEdges (cube, [])

checkCross :: VJRubiksCube -> Bool
checkCross cube = 
  let topPieces = getPieces (lookup Top cube) in
    if ((getNth 1 topPieces == Yellow) &&
        (getNth 3 topPieces == Yellow) &&
        (getNth 5 topPieces == Yellow) &&
        (getNth 7 topPieces == Yellow)) then True else False

checkExtendedCross :: VJRubiksCube -> Bool
checkExtendedCross cube = 
  let frontPieces = getPieces (lookup Front cube) in
    let rightPieces = getPieces (lookup Right cube) in
      let leftPieces = getPieces (lookup Left cube) in
        let backPieces = getPieces (lookup Back cube) in
          if ((getNth 1 frontPieces == Orange) &&
              (getNth 1 rightPieces == Blue) &&
              (getNth 1 leftPieces == Green) &&
              (getNth 1 backPieces == Red)) &&
              (checkCross cube == True) then True else False

fixYellowEdges :: (VJRubiksCube, [Move]) -> (VJRubiksCube, [Move])
fixYellowEdges (cube, moves) = 
  let (cube', moves') = fixFrontYellowEdges (cube, moves) (getPieces (lookup Front cube)) in
    let (cube'', moves'') = fixRightYellowEdges (cube', moves') (getPieces (lookup Right cube')) in
      let (cube''', moves''') = fixLeftYellowEdges (cube'', moves'') (getPieces (lookup Left cube'')) in
        fixBackYellowEdges (cube''', moves''') (getPieces (lookup Back cube'''))

fixFrontYellowEdges :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixFrontYellowEdges (cube, moves) pieces 
  | getNth 1 pieces == Yellow                        = fixFrontYellowEdges (yellowFrontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Front) Front) (getPieces (lookup Front (fst (yellowFrontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Front) Front))))
  | getNth 3 pieces == Yellow                        = fixFrontYellowEdges (yellowDownFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Front) Front) (getPieces (lookup Front (fst (yellowDownFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Front) Front))))
  | getNth 5 pieces == Yellow                        = fixFrontYellowEdges (yellowDownFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Front) Front) (getPieces (lookup Front (fst (yellowDownFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Front) Front))))
  | getNth 7 pieces == Yellow                        = fixFrontYellowEdges (yellowFrontFaceEdgeElevationCase (cube, moves) Front) (getPieces (lookup Front (fst (yellowFrontFaceEdgeElevationCase (cube, moves) Front))))
  | getNth 1 (getPieces(lookup Down cube)) == Yellow = fixFrontYellowEdges (yellowDownFaceEdgeElevationCase (cube, moves) Front) (getPieces (lookup Front (fst (yellowDownFaceEdgeElevationCase (cube, moves) Front))))
  | otherwise                                        = (cube, moves)

fixRightYellowEdges :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixRightYellowEdges (cube, moves) pieces 
  | getNth 1 pieces == Yellow                        = fixRightYellowEdges (yellowFrontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Right) Right) (getPieces (lookup Right (fst (yellowFrontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Right) Right))))
  | getNth 3 pieces == Yellow                        = fixRightYellowEdges (yellowDownFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Right) Right) (getPieces (lookup Right (fst (yellowDownFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Right) Right))))
  | getNth 5 pieces == Yellow                        = fixRightYellowEdges (yellowDownFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Right) Right) (getPieces (lookup Right (fst (yellowDownFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Right) Right))))
  | getNth 7 pieces == Yellow                        = fixRightYellowEdges (yellowFrontFaceEdgeElevationCase (cube, moves) Right) (getPieces (lookup Right (fst (yellowFrontFaceEdgeElevationCase (cube, moves) Right))))
  | getNth 5 (getPieces(lookup Down cube)) == Yellow = fixRightYellowEdges (yellowDownFaceEdgeElevationCase (cube, moves) Right) (getPieces (lookup Right (fst (yellowDownFaceEdgeElevationCase (cube, moves) Right))))
  | otherwise                                        = (cube, moves)
 
fixLeftYellowEdges :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move])
fixLeftYellowEdges (cube, moves) pieces 
  | getNth 1 pieces == Yellow                        = fixLeftYellowEdges (yellowFrontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Left) Left) (getPieces (lookup Left (fst (yellowFrontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Left) Left))))
  | getNth 3 pieces == Yellow                        = fixLeftYellowEdges (yellowDownFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Left) Left) (getPieces (lookup Left (fst (yellowDownFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Left) Left))))
  | getNth 5 pieces == Yellow                        = fixLeftYellowEdges (yellowDownFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Left) Left) (getPieces (lookup Left (fst (yellowDownFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Left) Left))))
  | getNth 7 pieces == Yellow                        = fixLeftYellowEdges (yellowFrontFaceEdgeElevationCase (cube, moves) Left) (getPieces (lookup Left (fst (yellowFrontFaceEdgeElevationCase (cube, moves) Left))))
  | getNth 3 (getPieces(lookup Down cube)) == Yellow = fixLeftYellowEdges (yellowDownFaceEdgeElevationCase (cube, moves) Left) (getPieces (lookup Left (fst (yellowDownFaceEdgeElevationCase (cube, moves) Left))))
  | otherwise                                        = (cube, moves)

fixBackYellowEdges :: (VJRubiksCube, [Move]) -> [Piece] -> (VJRubiksCube, [Move]) 
fixBackYellowEdges (cube, moves) pieces  
  | getNth 1 pieces == Yellow                        = fixBackYellowEdges (yellowFrontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Back) Back) (getPieces (lookup Back (fst (yellowFrontFaceEdgeElevationCase (skillfulTwist1 (cube, moves) Back) Back))))
  | getNth 3 pieces == Yellow                        = fixBackYellowEdges (yellowDownFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Back) Back) (getPieces (lookup Back (fst (yellowDownFaceEdgeElevationCase (skillfulTwist3 (cube, moves) Back) Back))))
  | getNth 5 pieces == Yellow                        = fixBackYellowEdges (yellowDownFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Back) Back) (getPieces (lookup Back (fst (yellowDownFaceEdgeElevationCase (skillfulTwist5 (cube, moves) Back) Back))))
  | getNth 7 pieces == Yellow                        = fixBackYellowEdges (yellowFrontFaceEdgeElevationCase (cube, moves) Back) (getPieces (lookup Back (fst (yellowFrontFaceEdgeElevationCase (cube, moves) Back))))
  | getNth 7 (getPieces(lookup Down cube)) == Yellow = fixBackYellowEdges (yellowDownFaceEdgeElevationCase (cube, moves) Back) (getPieces (lookup Back (fst (yellowDownFaceEdgeElevationCase (cube, moves) Back))))
  | otherwise                                        = (cube, moves)

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
yellowFrontFaceEdgeElevationCase :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
yellowFrontFaceEdgeElevationCase (cube, moves) side = 
  let pieces = getPieces (lookup Down cube) in
    case side of
      Front -> if (getNth 1 pieces == getColorOfSide side) then (translateMove (translateMove (translateMove (translateMove cube side 2) side 6) side 9) side 7, moves ++ [addMove side 2, addMove side 6, addMove side 9, addMove side 7])
               else (yellowFrontFaceEdgeElevationCase (down cube, moves ++ [addMove side 2]) Right)
      Right -> if (getNth 5 pieces == getColorOfSide side) then (translateMove (translateMove (translateMove (translateMove cube side 2) side 6) side 9) side 7, moves ++ [addMove side 2, addMove side 6, addMove side 9, addMove side 7])
               else (yellowFrontFaceEdgeElevationCase (down cube, moves ++ [addMove side 2]) Back)
      Back  -> if (getNth 7 pieces == getColorOfSide side) then (translateMove (translateMove (translateMove (translateMove cube side 2) side 6) side 9) side 7, moves ++ [addMove side 2, addMove side 6, addMove side 9, addMove side 7])
               else (yellowFrontFaceEdgeElevationCase (down cube, moves ++ [addMove side 2]) Left)
      Left  -> if (getNth 3 pieces == getColorOfSide side) then (translateMove (translateMove (translateMove (translateMove cube side 2) side 6) side 9) side 7, moves ++ [addMove side 2, addMove side 6, addMove side 9, addMove side 7])
               else (yellowFrontFaceEdgeElevationCase (down cube, moves ++ [addMove side 2]) Front)

-- F, F
yellowDownFaceEdgeElevationCase :: (VJRubiksCube, [Move]) -> Side -> (VJRubiksCube, [Move])
yellowDownFaceEdgeElevationCase (cube, moves) side = 
  let pieces = getPieces (lookup side cube) in
    case side of
      Front -> if (getNth 7 pieces == getColorOfSide side) then (translateMove (translateMove cube side 8) side 8, moves ++ [addMove side 8, addMove side 8])
               else (yellowDownFaceEdgeElevationCase (down cube, moves ++ [addMove side 2]) Right)
      Right -> if (getNth 7 pieces == getColorOfSide side) then (translateMove (translateMove cube side 8) side 8, moves ++ [addMove side 8, addMove side 8])
               else (yellowDownFaceEdgeElevationCase (down cube, moves ++ [addMove side 2]) Back)
      Back  -> if (getNth 7 pieces == getColorOfSide side) then (translateMove (translateMove cube side 8) side 8, moves ++ [addMove side 8, addMove side 8])
               else (yellowDownFaceEdgeElevationCase (down cube, moves ++ [addMove side 2]) Left)
      Left  -> if (getNth 7 pieces == getColorOfSide side) then (translateMove (translateMove cube side 8) side 8, moves ++ [addMove side 8, addMove side 8])
               else (yellowDownFaceEdgeElevationCase (down cube, moves ++ [addMove side 2]) Front)

getColorOfSide :: Side -> Color
getColorOfSide side = case side of
  Top   -> Yellow
  Front -> Orange
  Left  -> Green
  Back  -> Red
  Right -> Blue
  Down  -> White

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
