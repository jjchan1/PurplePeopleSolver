import Prelude hiding (Left, Right)

data Color
  = Blue | Green | Orange | Red | White | Yellow
  deriving (Eq, Show)

-- data Position
--   = TopLeft | TopMiddle | TopRight | MiddleLeft | MiddleMiddle | MiddleRight | BottomLeft | BottomMiddle | BottomRight
  -- deriving (Eq, Show)

data Side
  = Back | Front | Down | Top | Left | Right
  deriving (Eq, Show)

-- type Piece = (Position, Color)
type Piece = Color

type Face = (Side, [Piece])

-- type RubiksCube = [(Side, [Piece])]
type RubiksCube = [Face]

-- Moves
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs
   
moveThree s1 s2 s3 d1 d2 d3 src dst = 
  let a = replaceNth d1 (src!!s1) dst in
    let a' = replaceNth d2 (src!!s2) a in
      let a'' = replaceNth d3 (src!!s3) a' in
        a''   

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
    let t'' = moveThree 6 3 0 0 1 2 t t' in
    let t''' = moveThree 7 4 1 3 4 5 t t'' in
    let t'''' = moveThree 8 5 2 6 7 8 t t''' in
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
    let t'' = moveThree 2 5 8 0 1 2 t t' in
    let t''' = moveThree 1 4 7 3 4 5 t t'' in
    let t'''' = moveThree 0 3 6 6 7 8 t t''' in
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
    let d'' = moveThree 6 3 0 0 1 2 d d' in
    let d''' = moveThree 7 4 1 3 4 5 d d'' in
    let d'''' = moveThree 8 5 2 6 7 8 d d''' in
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
    let d'' = moveThree 2 5 8 0 1 2 d d' in
    let d''' = moveThree 1 4 7 3 4 5 d d'' in
    let d'''' = moveThree 0 3 6 6 7 8 d d''' in
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
    let t'' = moveThree 2 5 8 6 3 0 b t' in
    let b'' = moveThree 0 3 6 2 5 8 d b' in
    let d'' = moveThree 0 3 6 0 3 6 f d' in
    let l'' = moveThree 6 3 0 0 1 2 l l' in
    let l''' = moveThree 7 4 1 3 4 5 l l'' in
    let l'''' = moveThree 8 5 2 6 7 8 l l''' in
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
    let l'' = moveThree 2 5 8 0 1 2 l l' in
    let l''' = moveThree 1 4 7 3 4 5 l l'' in
    let l'''' = moveThree 0 3 6 6 7 8 l l''' in
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
    let b'' = moveThree 0 1 2 0 3 6 t b' in
    let d'' = moveThree 2 5 8 8 5 2 b d' in
    let r'' = moveThree 6 3 0 0 1 2 r r' in
    let r''' = moveThree 7 4 1 3 4 5 r r'' in
    let r'''' = moveThree 8 5 2 6 7 8 r r''' in
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
    let b'' = moveThree 2 5 8 6 3 0 d b' in
    let d'' = moveThree 2 5 8 2 5 8 f d' in
    let r'' = moveThree 2 5 8 0 1 2 r r' in
    let r''' = moveThree 1 4 7 3 4 5 r r'' in
    let r'''' = moveThree 0 3 6 6 7 8 r r''' in
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
    let l'' = moveThree 2 1 0 8 5 2 d l' in
    let d'' = moveThree 0 3 6 2 1 0 r d' in
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
    let r'' = moveThree 0 1 2 6 3 0 d r' in
    let t'' = moveThree 0 3 6 6 7 8 r t' in
    let l'' = moveThree 6 7 8 8 5 2 t l' in
    let d'' = moveThree 2 5 8 0 1 2 l d' in
    let f'' = moveThree 2 5 8 0 1 2 f f' in
    let f''' = moveThree 1 4 7 3 4 5 f f'' in
    let f'''' = moveThree 0 3 6 6 7 8 f f''' in
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
    let r'' = moveThree 6 7 8 8 5 2 d r' in
    let t'' = moveThree 2 5 8 0 1 2 r t' in
    let l'' = moveThree 0 1 2 6 3 0 t l' in
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
    let d'' = moveThree 2 5 8 8 7 6 r d' in
    let b'' = moveThree 0 1 2 6 3 0 b b' in
    let b''' = moveThree 3 4 5 7 4 1 b b'' in
    let b'''' = moveThree 6 7 8 8 5 2 b b''' in
      [(Front, f), (Back, b''''), (Left, l''), (Right, r''), (Top, t''), (Down, d'')]      

 

-- movePieces cube s1 s2 s3 d1 d2 d3 src dst =
--   let x = src!!s1 in
--   let y = src!!s2 in
--   let z = src!!s3 in

--   src!!s1 = dst!!d1
--   src!!s2 = dst!!d2
--   src!!s3 = dst!!d3

--   dst!!d1 = x
--   dst!!d2 = y
--   dst!!d3 = z




getPieces :: Maybe [Piece] -> [Piece]
getPieces pieces =
  case pieces of
    Nothing -> []
    Just p -> p

solvedCube :: RubiksCube
solvedCube = 
  [(Down,[White,White,White,White,White,White,White,White,White]),
  (Top,[Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow]),
  (Left,[Green,Green,Green,Green,Green,Green,Green,Green,Green]),
  (Right,[Blue,Blue,Blue,Blue,Blue,Blue,Blue,Blue,Blue]),
  (Front,[Orange,Orange,Orange,Orange,Orange,Orange,Orange,Orange,Orange]),
  (Back,[Red,Red,Red,Red,Red,Red,Red,Red,Red])]

--   [(Top,[(TopLeft,    White), (TopMiddle,    White), (TopRight,    White),
--          (MiddleLeft, White), (MiddleMiddle, White), (MiddleRight, White),
--          (BottomLeft, White), (BottomMiddle, White), (BottomRight, White)]),
--    (Down,[(TopLeft,    Yellow), (TopMiddle,    Yellow), (TopRight,    Yellow),
--           (MiddleLeft, Yellow), (MiddleMiddle, Yellow), (MiddleRight, Yellow),
--           (BottomLeft, Yellow), (BottomMiddle, Yellow), (BottomRight, Yellow)]),
--    (Left,[(TopLeft,    Green), (TopMiddle,    Green), (TopRight,    Green),
--           (MiddleLeft, Green), (MiddleMiddle, Green), (MiddleRight, Green),
--           (BottomLeft, Green), (BottomMiddle, Green), (BottomRight, Green)]),
--    (Right,[(TopLeft,    Blue), (TopMiddle,    Blue), (TopRight,    Blue),
--            (MiddleLeft, Blue), (MiddleMiddle, Blue), (MiddleRight, Blue),
--            (BottomLeft, Blue), (BottomMiddle, Blue), (BottomRight, Blue)]),
--    (Back,[(TopLeft,    Orange), (TopMiddle,    Orange), (TopRight,    Orange),
--           (MiddleLeft, Orange), (MiddleMiddle, Orange), (MiddleRight, Orange),
--           (BottomLeft, Orange), (BottomMiddle, Orange), (BottomRight, Orange)]),
--    (Front,[(TopLeft,    Red), (TopMiddle,    Red), (TopRight,    Red),
--            (MiddleLeft, Red), (MiddleMiddle, Red), (MiddleRight, Red),
--            (BottomLeft, Red), (BottomMiddle, Red), (BottomRight, Red)])]






