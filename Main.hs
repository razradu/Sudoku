module Main where

import HaskellSay (haskellSay)
import qualified System.Process as SP

clearScreen :: IO ()
clearScreen = do 
  _ <- SP.system "reset"
  return ()

sudokuMsg :: IO ()
sudokuMsg = do
  putStrLn "The Sudoku game works with a 9 x 9 board. Each cell can contain a number from 1 to 9."
  putStrLn " The number in each cell must be unique in its line, column and smaller square."
  putStrLn "  The board comes with few already filled in positions. Games courtesy of https://www.websudoku.com/"
  putStrLn "Good luck!"
  putStrLn "____________________________________________________________________________________________________"
  putStrLn ""
  putStrLn ""
  

type Row    = [Int]
type Board  = [Row]


getRow :: Int -> Board -> Row
getRow n xss = xss !! n

getColumn :: Int -> Board -> Row
getColumn n board = getRow n (transpose board)

getNo :: (Int, Int) -> Board -> Int
getNo (m, n) xss = xss !! m !! n

showNo :: Int -> String
showNo 0 = "-"
showNo n = show n

transpose :: Board -> Board
transpose ([] : _)  = []
transpose xss       = (map head xss) : (transpose (map tail xss))

putNo :: Int -> Int -> Row -> Row
putNo no 0 (x : xs)  = no : xs
putNo no n (x : xs)  = x : (putNo no (n - 1) xs)

playMove :: Int -> (Int, Int) -> Board -> Board
playMove no (0, n) (xs : xss) = (putNo no n xs) : xss
playMove no (m, n) (xs : xss) = xs : (playMove no (m - 1, n) xss)

smallSquareCorner :: (Int, Int) -> (Int, Int)
smallSquareCorner (m,n) = (m - (m `mod` 3), n - (n `mod` 3))

smallSquareList :: (Int, Int) -> Board -> [Int]
smallSquareList (m, n) board = [ getNo (i, j) board | i <- [m .. m+2], j <- [n .. n+2]]

smallSquare :: (Int, Int) -> Board -> [Int]
smallSquare = smallSquareList . smallSquareCorner

check :: (Int, Int) -> Int -> Board -> Bool
check (m, n) no board =
  getNo (m, n) board == 0 &&
  not (elem no (getRow m board)) &&
  not (elem no (getColumn n board)) &&
  not (elem no (smallSquare (m, n) board))

isBoardFull :: Board -> Bool
isBoardFull b = all (/= 0) (concat b)

showRow :: Row -> IO ()
showRow [x]       = do
  putStr $ showNo x
  putStr "\n"
showRow (x : xs)  = do
  putStr $ showNo x
  putChar ' '
  showRow xs

showBoard :: Board -> IO ()
showBoard = sequence_ . (map showRow)

playGame :: Board -> IO ()
playGame b = do
  showBoard b
  putStrLn " "
  putStrLn $ "Enter position & number in format: (line, column, number)"
  line <- getLine
  (x, y, n) <- readIO line :: IO (Int, Int, Int)

-- if elem x [0..8] && elem y [0..8] && elem n [1..9]
--  then

  case check (x, y) n b of
    True -> do
      clearScreen
      sudokuMsg
      let c = playMove n (x, y) b
      case isBoardFull c of
        True      -> putStrLn "Game completed. Congratulations !!"
        otherwise -> playGame c
    False -> do
      case getNo (x, y) b of
        0 -> do
          clearScreen
          sudokuMsg
          putStrLn "Number not satisfying conditions in that position!"
          putStrLn " "
          playGame b
        otherwise -> do
          clearScreen
          sudokuMsg
          putStrLn $ "Position (" ++ (show x) ++ ", " ++ (show y) ++ ") already filled"
          playGame b

main :: IO ()
main = do

  clearScreen
  sudokuMsg
  
  putStrLn "Which game do you want to play (1 - 10)?"
  putStrLn "1 - 3 Easy | 4 - 6 Medium | 7 - 9 Hard | 10 Evil"
  s <- getLine
  sudoku <- readIO s :: IO Int
  putStrLn $ "Playing game number " ++ (show sudoku)

  game <- readFile $ "./games/game" ++ (show sudoku)

  let board = read game

  playGame board

