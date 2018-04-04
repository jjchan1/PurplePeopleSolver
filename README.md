# RubiksCubeSolver
<img src = "https://azscitech.com//wp-content/uploads/2014/05/rubikslogo.jpg">

## Project Overview
Authors: Jonathan Chan, Vivian Yung

http://bit.ly/2ypoYC6

__Project Goals:__
* To create a Rubik's cube solver in Haskell using native libraries
  * Users will enter a pre-shuffled Rubik's cube or have the program randomly shuffle a cube for them.
  * The solver will output the steps required to solve the cube.

__How to run:__
* Download repository and place all files into one folder.
* In Command Prompt/Terminal, navigate to the project folder.
* Run "ghc --make Main.hs" (without quotations)
* Run "Main -o solution.svg -w 10000 -h 500" (without quotations)
  * The arguments for -w and -h can be adjusted
  * The solution will be called solution.svg and will be in the project folder.
