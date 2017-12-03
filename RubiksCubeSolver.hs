import Prelude hiding (Left, Right)
import System.Random
import Data.List

data Color
  = Blue | Green | Orange | Red | White | Yellow
  deriving (Eq, Show)

data Side
  = Back | Front | Down | Top | Left | Right
  deriving (Eq, Show)

type Piece = Color

type Face = (Side, [Piece])

type RubiksCube = [Face]

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

up :: RubiksCube -> RubiksCube
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

up' :: RubiksCube -> RubiksCube
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

down :: RubiksCube -> RubiksCube
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

down' :: RubiksCube -> RubiksCube
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

left :: RubiksCube -> RubiksCube
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

left' :: RubiksCube -> RubiksCube
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

right :: RubiksCube -> RubiksCube
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

right' :: RubiksCube -> RubiksCube
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

front :: RubiksCube -> RubiksCube
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

front' :: RubiksCube -> RubiksCube
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

back :: RubiksCube -> RubiksCube
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

back' :: RubiksCube -> RubiksCube
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

solvedCube :: RubiksCube
solvedCube = 
  [(Down,[White,White,White,White,White,White,White,White,White]),
  (Top,[Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow]),
  (Left,[Green,Green,Green,Green,Green,Green,Green,Green,Green]),
  (Right,[Blue,Blue,Blue,Blue,Blue,Blue,Blue,Blue,Blue]),
  (Front,[Orange,Orange,Orange,Orange,Orange,Orange,Orange,Orange,Orange]),
  (Back,[Red,Red,Red,Red,Red,Red,Red,Red,Red])]

doMove :: Int -> RubiksCube -> RubiksCube
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

scramble :: RubiksCube -> IO RubiksCube
scramble cube = do
  seed <- newStdGen
  let list = randomlist 25 seed
  let alteredCube = foldl (\c n -> doMove n c) cube list
  return alteredCube

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . randomR(0, 11))

solveYellowCross' :: RubiksCube -> RubiksCube
solveYellowCross' cube = case checkExtendedCross cube of
  True -> cube
  False -> fixYellowEdges cube 

-- solveYellowCross :: (RubiksCube, [Int]) -> (RubiksCube, [Int])
-- solveYellowCross (cube, moves) = case checkExtendedCross cube of
--   True -> (cube, moves)
--   False -> fixYellowEdges cube (getPieces)

checkCross :: RubiksCube -> Bool
checkCross cube = 
  let topPieces = getPieces (lookup Top cube) in
    if ((getNth 1 topPieces == Yellow) &&
        (getNth 3 topPieces == Yellow) &&
        (getNth 5 topPieces == Yellow) &&
        (getNth 7 topPieces == Yellow)) then True else False

checkExtendedCross :: RubiksCube -> Bool
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

fixYellowEdges :: RubiksCube -> RubiksCube
fixYellowEdges cube = 
  let cube' = fixFrontYellowEdges cube (getPieces (lookup Front cube)) in
    let cube'' = fixRightYellowEdges cube' (getPieces (lookup Right cube')) in
      let cube''' = fixLeftYellowEdges cube'' (getPieces (lookup Left cube'')) in
        fixBackYellowEdges cube''' (getPieces (lookup Back cube'''))

fixFrontYellowEdges :: RubiksCube -> [Piece] -> RubiksCube
fixFrontYellowEdges cube pieces 
  | getNth 1 pieces == Yellow                        = fixFrontYellowEdges (yellowFrontFaceEdgeElevationCase (skillfulTwist1 cube Front) Front) (getPieces (lookup Front (yellowFrontFaceEdgeElevationCase (skillfulTwist1 cube Front) Front)))
  | getNth 3 pieces == Yellow                        = fixFrontYellowEdges (yellowDownFaceEdgeElevationCase (skillfulTwist3 cube Front) Front) (getPieces (lookup Front (yellowDownFaceEdgeElevationCase (skillfulTwist3 cube Front) Front)))
  | getNth 5 pieces == Yellow                        = fixFrontYellowEdges (yellowDownFaceEdgeElevationCase (skillfulTwist5 cube Front) Front) (getPieces (lookup Front (yellowDownFaceEdgeElevationCase (skillfulTwist5 cube Front) Front)))
  | getNth 7 pieces == Yellow                        = fixFrontYellowEdges (yellowFrontFaceEdgeElevationCase cube Front) (getPieces (lookup Front (yellowFrontFaceEdgeElevationCase cube Front)))
  | getNth 1 (getPieces(lookup Down cube)) == Yellow = fixFrontYellowEdges (yellowDownFaceEdgeElevationCase cube Front) (getPieces (lookup Front (yellowDownFaceEdgeElevationCase cube Front)))
  | otherwise                                        = cube

fixRightYellowEdges :: RubiksCube -> [Piece] -> RubiksCube
fixRightYellowEdges cube pieces 
  | getNth 1 pieces == Yellow                        = fixRightYellowEdges (yellowFrontFaceEdgeElevationCase (skillfulTwist1 cube Right) Right) (getPieces (lookup Right (yellowFrontFaceEdgeElevationCase (skillfulTwist1 cube Right) Right)))
  | getNth 3 pieces == Yellow                        = fixRightYellowEdges (yellowDownFaceEdgeElevationCase (skillfulTwist3 cube Right) Right) (getPieces (lookup Right (yellowDownFaceEdgeElevationCase (skillfulTwist3 cube Right) Right)))
  | getNth 5 pieces == Yellow                        = fixRightYellowEdges (yellowDownFaceEdgeElevationCase (skillfulTwist5 cube Right) Right) (getPieces (lookup Right (yellowDownFaceEdgeElevationCase (skillfulTwist5 cube Right) Right)))
  | getNth 7 pieces == Yellow                        = fixRightYellowEdges (yellowFrontFaceEdgeElevationCase cube Right) (getPieces (lookup Right (yellowFrontFaceEdgeElevationCase cube Right)))
  | getNth 5 (getPieces(lookup Down cube)) == Yellow = fixRightYellowEdges (yellowDownFaceEdgeElevationCase cube Right) (getPieces (lookup Right (yellowDownFaceEdgeElevationCase cube Right)))
  | otherwise                                        = cube
 
fixLeftYellowEdges :: RubiksCube -> [Piece] -> RubiksCube
fixLeftYellowEdges cube pieces 
  | getNth 1 pieces == Yellow                        = fixLeftYellowEdges (yellowFrontFaceEdgeElevationCase (skillfulTwist1 cube Left) Left) (getPieces (lookup Left (yellowFrontFaceEdgeElevationCase (skillfulTwist1 cube Left) Left)))
  | getNth 3 pieces == Yellow                        = fixLeftYellowEdges (yellowDownFaceEdgeElevationCase (skillfulTwist3 cube Left) Left) (getPieces (lookup Left (yellowDownFaceEdgeElevationCase (skillfulTwist3 cube Left) Left)))
  | getNth 5 pieces == Yellow                        = fixLeftYellowEdges (yellowDownFaceEdgeElevationCase (skillfulTwist5 cube Left) Left) (getPieces (lookup Left (yellowDownFaceEdgeElevationCase (skillfulTwist5 cube Left) Left)))
  | getNth 7 pieces == Yellow                        = fixLeftYellowEdges (yellowFrontFaceEdgeElevationCase cube Left) (getPieces (lookup Left (yellowFrontFaceEdgeElevationCase cube Left)))
  | getNth 3 (getPieces(lookup Down cube)) == Yellow = fixLeftYellowEdges (yellowDownFaceEdgeElevationCase cube Left) (getPieces (lookup Left (yellowDownFaceEdgeElevationCase cube Left)))
  | otherwise                                        = cube 

fixBackYellowEdges :: RubiksCube -> [Piece] -> RubiksCube 
fixBackYellowEdges cube pieces  
  | getNth 1 pieces == Yellow                        = fixBackYellowEdges (yellowFrontFaceEdgeElevationCase (skillfulTwist1 cube Back) Back) (getPieces (lookup Back (yellowFrontFaceEdgeElevationCase (skillfulTwist1 cube Back) Back)))
  | getNth 3 pieces == Yellow                        = fixBackYellowEdges (yellowDownFaceEdgeElevationCase (skillfulTwist3 cube Back) Back) (getPieces (lookup Back (yellowDownFaceEdgeElevationCase (skillfulTwist3 cube Back) Back)))
  | getNth 5 pieces == Yellow                        = fixBackYellowEdges (yellowDownFaceEdgeElevationCase (skillfulTwist5 cube Back) Back) (getPieces (lookup Back (yellowDownFaceEdgeElevationCase (skillfulTwist5 cube Back) Back)))
  | getNth 7 pieces == Yellow                        = fixBackYellowEdges (yellowFrontFaceEdgeElevationCase cube Back) (getPieces (lookup Back (yellowFrontFaceEdgeElevationCase cube Back)))
  | getNth 7 (getPieces(lookup Down cube)) == Yellow = fixBackYellowEdges (yellowDownFaceEdgeElevationCase cube Back) (getPieces (lookup Back (yellowDownFaceEdgeElevationCase cube Back)))
  | otherwise                                        = cube

skillfulTwist1 :: RubiksCube -> Side -> RubiksCube
skillfulTwist1 cube side =
  translateMove (translateMove cube side 8) side 8

skillfulTwist3 :: RubiksCube -> Side -> RubiksCube
skillfulTwist3 cube side =
  translateMove (translateMove (translateMove cube side 4) side 2) side 5

skillfulTwist5 :: RubiksCube -> Side -> RubiksCube
skillfulTwist5 cube side =
  translateMove (translateMove (translateMove cube side 7) side 3) side 6

-- needs new side after turning
yellowFrontFaceEdgeElevationCase :: RubiksCube -> Side -> RubiksCube
yellowFrontFaceEdgeElevationCase cube side = 
  let pieces = getPieces (lookup Down cube) in
    case side of
      Front -> if (getNth 1 pieces == getColorOfSide side) then (translateMove (translateMove (translateMove (translateMove cube side 2) side 6) side 9) side 7)
               else (yellowFrontFaceEdgeElevationCase (down cube) Right)
      Right -> if (getNth 5 pieces == getColorOfSide side) then (translateMove (translateMove (translateMove (translateMove cube side 2) side 6) side 9) side 7)
               else (yellowFrontFaceEdgeElevationCase (down cube) Back)
      Back  -> if (getNth 7 pieces == getColorOfSide side) then (translateMove (translateMove (translateMove (translateMove cube side 2) side 6) side 9) side 7)
               else (yellowFrontFaceEdgeElevationCase (down cube) Left)
      Left  -> if (getNth 3 pieces == getColorOfSide side) then (translateMove (translateMove (translateMove (translateMove cube side 2) side 6) side 9) side 7)
               else (yellowFrontFaceEdgeElevationCase (down cube) Front)

-- needs new side after turning
yellowDownFaceEdgeElevationCase :: RubiksCube -> Side -> RubiksCube
yellowDownFaceEdgeElevationCase cube side = 
  let pieces = getPieces (lookup side cube) in
    case side of
      Front -> if (getNth 7 pieces == getColorOfSide side) then (translateMove (translateMove cube side 8) side 8)
               else (yellowDownFaceEdgeElevationCase (down cube) Right)
      Right -> if (getNth 7 pieces == getColorOfSide side) then (translateMove (translateMove cube side 8) side 8)
               else (yellowDownFaceEdgeElevationCase (down cube) Back)
      Back  -> if (getNth 7 pieces == getColorOfSide side) then (translateMove (translateMove cube side 8) side 8)
               else (yellowDownFaceEdgeElevationCase (down cube) Left)
      Left  -> if (getNth 7 pieces == getColorOfSide side) then (translateMove (translateMove cube side 8) side 8)
               else (yellowDownFaceEdgeElevationCase (down cube) Front)

-- yellowFrontFaceEdgeElevationCase :: RubiksCube -> Side -> RubiksCube
-- yellowFrontFaceEdgeElevationCase cube side = 
--   let pieces = getPieces (lookup Down cube) in
--   -- 1 position might be wrong
--     if (getNth 1 pieces == getColorOfSide side) then (right' (front' (right (down cube))))
--     else (yellowFrontFaceEdgeElevationCase (down cube) side)

-- yellowDownFaceEdgeElevationCase :: RubiksCube -> Side -> RubiksCube
-- yellowDownFaceEdgeElevationCase cube side = 
--   let pieces = getPieces (lookup side cube) in
--   -- needs translated front (front cube)
--     if (getNth 7 pieces == getColorOfSide side) then (front (front cube))
--     else (yellowDownFaceEdgeElevationCase (down cube) side)

getColorOfSide :: Side -> Color
getColorOfSide side = case side of
  Top   -> Yellow
  Front -> Orange
  Left  -> Green
  Back  -> Red
  Right -> Blue
  Down  -> White

translateMove :: RubiksCube -> Side -> Int -> RubiksCube
translateMove cube frontSide move =
  case frontSide of
    Left -> case move of
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
    Right -> case move of 
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
       