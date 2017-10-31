import Prelude hiding (Left, Right)

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
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs
   
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
    Just p -> p

up :: RubiksCube -> RubiksCube
up cube = 
  let b = getPieces (lookup Back cube) in
  let b' = b in
  let f = getPieces (lookup Front cube) in
  let f' = f in
  let t = getPieces (lookup Top cube) in
  let t' = t in 
  let l = getPieces (lookup Left cube) in
  let l' = l in 
  let r = getPieces (lookup Right cube) in 
  let r' = r in
  let d = getPieces (lookup Down cube) in 
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
  let b' = b in
  let f = getPieces (lookup Front cube) in
  let f' = f in
  let t = getPieces (lookup Top cube) in
  let t' = t in 
  let l = getPieces (lookup Left cube) in
  let l' = l in 
  let r = getPieces (lookup Right cube) in 
  let r' = r in
  let d = getPieces (lookup Down cube) in 
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
  let b' = b in
  let f = getPieces (lookup Front cube) in
  let f' = f in
  let t = getPieces (lookup Top cube) in
  let l = getPieces (lookup Left cube) in
  let l' = l in 
  let r = getPieces (lookup Right cube) in 
  let r' = r in
  let d = getPieces (lookup Down cube) in
  let d' = d in
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
  let b' = b in
  let f = getPieces (lookup Front cube) in
  let f' = f in
  let t = getPieces (lookup Top cube) in
  let l = getPieces (lookup Left cube) in
  let l' = l in 
  let r = getPieces (lookup Right cube) in 
  let r' = r in
  let d = getPieces (lookup Down cube) in
  let d' = d in
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
  let b' = b in
  let f = getPieces (lookup Front cube) in
  let f' = f in
  let t = getPieces (lookup Top cube) in
  let t' = t in
  let l = getPieces (lookup Left cube) in
  let l' = l in 
  let r = getPieces (lookup Right cube) in 
  let d = getPieces (lookup Down cube) in
  let d' = d in
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
  let b' = b in
  let f = getPieces (lookup Front cube) in
  let f' = f in
  let t = getPieces (lookup Top cube) in
  let t' = t in
  let l = getPieces (lookup Left cube) in
  let l' = l in 
  let r = getPieces (lookup Right cube) in 
  let d = getPieces (lookup Down cube) in
  let d' = d in
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
  let b' = b in
  let f = getPieces (lookup Front cube) in
  let f' = f in
  let t = getPieces (lookup Top cube) in
  let t' = t in
  let l = getPieces (lookup Left cube) in
  let r = getPieces (lookup Right cube) in 
  let r' = r in
  let d = getPieces (lookup Down cube) in
  let d' = d in
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
  let b' = b in
  let f = getPieces (lookup Front cube) in
  let f' = f in
  let t = getPieces (lookup Top cube) in
  let t' = t in
  let l = getPieces (lookup Left cube) in
  let r = getPieces (lookup Right cube) in 
  let r' = r in
  let d = getPieces (lookup Down cube) in
  let d' = d in
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
  let f' = f in
  let t = getPieces (lookup Top cube) in
  let t' = t in
  let l = getPieces (lookup Left cube) in
  let l' = l in
  let r = getPieces (lookup Right cube) in 
  let r' = r in
  let d = getPieces (lookup Down cube) in
  let d' = d in
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
  let f' = f in
  let t = getPieces (lookup Top cube) in
  let t' = t in
  let l = getPieces (lookup Left cube) in
  let l' = l in
  let r = getPieces (lookup Right cube) in 
  let r' = r in
  let d = getPieces (lookup Down cube) in
  let d' = d in
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
  let b' = b in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Top cube) in
  let t' = t in
  let l = getPieces (lookup Left cube) in
  let l' = l in
  let r = getPieces (lookup Right cube) in 
  let r' = r in
  let d = getPieces (lookup Down cube) in
  let d' = d in
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
  let b' = b in
  let f = getPieces (lookup Front cube) in
  let t = getPieces (lookup Top cube) in
  let t' = t in
  let l = getPieces (lookup Left cube) in
  let l' = l in
  let r = getPieces (lookup Right cube) in 
  let r' = r in
  let d = getPieces (lookup Down cube) in
  let d' = d in
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






