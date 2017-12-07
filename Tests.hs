module Tests where

import Types
import Moves
import Main

import System.Environment
import Data.Monoid
import Debug.Trace

import Prelude hiding (Left, Right)
import Diagrams.Prelude hiding (Color, up, up', down, down', left, left', right, right', front, front', back, back')
import Diagrams.RubiksCube.Model hiding (Side)

checkUpTest :: Bool
checkUpTest =
  up solvedCube == (([(Front,[Blue,Blue,Blue,Orange,Orange,Orange,Orange,Orange,Orange]),
                      (Back,[Green,Green,Green,Red,Red,Red,Red,Red,Red]),
                      (Left,[Orange,Orange,Orange,Green,Green,Green,Green,Green,Green]),
                      (Right,[Red,Red,Red,Blue,Blue,Blue,Blue,Blue,Blue]),
                      (Up,[Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow]),
                      (Down,[White,White,White,White,White,White,White,White,White])])::VJRubiksCube)

checkUpTest' :: Bool
checkUpTest' =
  up' solvedCube == (([(Front,[Green,Green,Green,Orange,Orange,Orange,Orange,Orange,Orange]),
                       (Back,[Blue,Blue,Blue,Red,Red,Red,Red,Red,Red]),
                       (Left,[Red,Red,Red,Green,Green,Green,Green,Green,Green]),
                       (Right,[Orange,Orange,Orange,Blue,Blue,Blue,Blue,Blue,Blue]),
                       (Up,[Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow]),
                       (Down,[White,White,White,White,White,White,White,White,White])])::VJRubiksCube)

checkDownTest :: Bool
checkDownTest =
  down solvedCube == (([(Front,[Orange,Orange,Orange,Orange,Orange,Orange,Green,Green,Green]),
                        (Back,[Red,Red,Red,Red,Red,Red,Blue,Blue,Blue]),
                        (Left,[Green,Green,Green,Green,Green,Green,Red,Red,Red]),
                        (Right,[Blue,Blue,Blue,Blue,Blue,Blue,Orange,Orange,Orange]),
                        (Up,[Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow]),
                        (Down,[White,White,White,White,White,White,White,White,White])])::VJRubiksCube)

checkDownTest' :: Bool
checkDownTest' =
  down' solvedCube == (([(Front,[Orange,Orange,Orange,Orange,Orange,Orange,Blue,Blue,Blue]),
                         (Back,[Red,Red,Red,Red,Red,Red,Green,Green,Green]),
                         (Left,[Green,Green,Green,Green,Green,Green,Orange,Orange,Orange]),
                         (Right,[Blue,Blue,Blue,Blue,Blue,Blue,Red,Red,Red]),
                         (Up,[Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow]),
                         (Down,[White,White,White,White,White,White,White,White,White])])::VJRubiksCube)

checkLeftTest :: Bool
checkLeftTest =
  left solvedCube == (([(Front,[Yellow,Orange,Orange,Yellow,Orange,Orange,Yellow,Orange,Orange]),
                        (Back,[Red,Red,White,Red,Red,White,Red,Red,White]),
                        (Left,[Green,Green,Green,Green,Green,Green,Green,Green,Green]),
                        (Right,[Blue,Blue,Blue,Blue,Blue,Blue,Blue,Blue,Blue]),
                        (Up,[Red,Yellow,Yellow,Red,Yellow,Yellow,Red,Yellow,Yellow]),
                        (Down,[Orange,White,White,Orange,White,White,Orange,White,White])])::VJRubiksCube)

checkLeftTest' :: Bool  
checkLeftTest' =
  left' solvedCube == (([(Front,[White,Orange,Orange,White,Orange,Orange,White,Orange,Orange]),
                         (Back,[Red,Red,Yellow,Red,Red,Yellow,Red,Red,Yellow]),
                         (Left,[Green,Green,Green,Green,Green,Green,Green,Green,Green]),
                         (Right,[Blue,Blue,Blue,Blue,Blue,Blue,Blue,Blue,Blue]),
                         (Up,[Orange,Yellow,Yellow,Orange,Yellow,Yellow,Orange,Yellow,Yellow]),
                         (Down,[Red,White,White,Red,White,White,Red,White,White])])::VJRubiksCube)

checkRightTest :: Bool
checkRightTest =
  right solvedCube == (([(Front,[Orange,Orange,White,Orange,Orange,White,Orange,Orange,White]),
                         (Back,[Yellow,Red,Red,Yellow,Red,Red,Yellow,Red,Red]),
                         (Left,[Green,Green,Green,Green,Green,Green,Green,Green,Green]),
                         (Right,[Blue,Blue,Blue,Blue,Blue,Blue,Blue,Blue,Blue]),
                         (Up,[Yellow,Yellow,Orange,Yellow,Yellow,Orange,Yellow,Yellow,Orange]),
                         (Down,[White,White,Red,White,White,Red,White,White,Red])])::VJRubiksCube)

checkRightTest' :: Bool
checkRightTest' =
  right' solvedCube == (([(Front,[Orange,Orange,Yellow,Orange,Orange,Yellow,Orange,Orange,Yellow]),
                          (Back,[White,Red,Red,White,Red,Red,White,Red,Red]),
                          (Left,[Green,Green,Green,Green,Green,Green,Green,Green,Green]),
                          (Right,[Blue,Blue,Blue,Blue,Blue,Blue,Blue,Blue,Blue]),
                          (Up,[Yellow,Yellow,Red,Yellow,Yellow,Red,Yellow,Yellow,Red]),
                          (Down,[White,White,Orange,White,White,Orange,White,White,Orange])])::VJRubiksCube)

checkFrontTest :: Bool
checkFrontTest =
  front solvedCube == (([(Front,[Orange,Orange,Orange,Orange,Orange,Orange,Orange,Orange,Orange]),
                         (Back,[Red,Red,Red,Red,Red,Red,Red,Red,Red]),
                         (Left,[Green,Green,White,Green,Green,White,Green,Green,White]),
                         (Right,[Yellow,Blue,Blue,Yellow,Blue,Blue,Yellow,Blue,Blue]),
                         (Up,[Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Green,Green,Green]),
                         (Down,[Blue,Blue,Blue,White,White,White,White,White,White])])::VJRubiksCube)

checkFrontTest' :: Bool
checkFrontTest' =
  front' solvedCube == (([(Front,[Orange,Orange,Orange,Orange,Orange,Orange,Orange,Orange,Orange]),
                          (Back,[Red,Red,Red,Red,Red,Red,Red,Red,Red]),
                          (Left,[Green,Green,Yellow,Green,Green,Yellow,Green,Green,Yellow]),
                          (Right,[White,Blue,Blue,White,Blue,Blue,White,Blue,Blue]),
                          (Up,[Yellow,Yellow,Yellow,Yellow,Yellow,Yellow,Blue,Blue,Blue]),
                          (Down,[Green,Green,Green,White,White,White,White,White,White])])::VJRubiksCube)

checkBackTest :: Bool
checkBackTest =
  back solvedCube == (([(Front,[Orange,Orange,Orange,Orange,Orange,Orange,Orange,Orange,Orange]),
                        (Back,[Red,Red,Red,Red,Red,Red,Red,Red,Red]),
                        (Left,[Yellow,Green,Green,Yellow,Green,Green,Yellow,Green,Green]),
                        (Right,[Blue,Blue,White,Blue,Blue,White,Blue,Blue,White]),
                        (Up,[Blue,Blue,Blue,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow]),
                        (Down,[White,White,White,White,White,White,Green,Green,Green])])::VJRubiksCube)

checkBackTest' :: Bool
checkBackTest' =
  back' solvedCube == (([(Front,[Orange,Orange,Orange,Orange,Orange,Orange,Orange,Orange,Orange]),
                         (Back,[Red,Red,Red,Red,Red,Red,Red,Red,Red]),
                         (Left,[White,Green,Green,White,Green,Green,White,Green,Green]),
                         (Right,[Blue,Blue,Yellow,Blue,Blue,Yellow,Blue,Blue,Yellow]),
                         (Up,[Green,Green,Green,Yellow,Yellow,Yellow,Yellow,Yellow,Yellow]),
                         (Down,[White,White,White,White,White,White,Blue,Blue,Blue])])::VJRubiksCube)

unitTests :: [(String, Bool)]
unitTests =
 [
   ("checkUp", checkUpTest),
   ("checkUp'", checkUpTest'),
   ("checkDown", checkDownTest),
   ("checkDown'", checkDownTest'),
   ("checkLeft", checkLeftTest),
   ("checkLeft'", checkLeftTest'),
   ("checkRight", checkRightTest),
   ("checkRight'", checkRightTest'),
   ("checkFront", checkFrontTest),
   ("checkFront'", checkFrontTest'),
   ("checkBack", checkBackTest),
   ("checkBack'", checkBackTest')
 ]

runTests :: String -> IO ()
runTests func =
  case (lookup func unitTests) of
    Just t -> if t then putStrLn $ "Passed " ++ func
              else error $ "Failed " ++ func
    Nothing -> error $ "Unrecognized test: " ++ func

runTestsAll :: [IO ()]
runTestsAll = 
  let tests = unitTests in 
    mapM_ (\x -> runTests (fst x)) (tests)

 -- printTests :: [IO ()] -> IO ()
 -- printTests

-- main = do
--   args <- getArgs
--   let func = head args in
--     runTests func