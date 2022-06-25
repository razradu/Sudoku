-- Sudoku.hs

type Row    = [Int]
type Board  = [Row]

board1 :: Board
board1 = [
 [0, 0, 0, 0, 4, 0, 0, 0, 9],
 [0, 1, 0, 7, 0, 0, 4, 0, 8],
 [3, 4, 0, 0, 1, 5, 2, 7, 6],
 [0, 0, 0, 0, 0, 9, 0, 8, 0],
 [6, 0, 1, 0, 8, 0, 5, 0, 4],
 [0, 2, 0, 1, 0, 0, 0, 0, 0],
 [1, 3, 4, 5, 9, 0, 0, 6, 7],
 [5, 0, 7, 0, 0, 8, 0, 1, 0],
 [2, 0, 0, 0, 7, 0, 0, 0, 0]
 ]

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
      let c = playMove n (x, y) b
      case isBoardFull c of
        True      -> putStrLn "Game completed. Congratulations !!"
        otherwise -> playGame c
    False -> do
      case getNo (x, y) b of
        0 -> do
          putStrLn "Number not satisfying conditions in that position!"
          putStrLn " "
          playGame b
        otherwise -> do
          putStrLn $ "Position (" ++ (show x) ++ ", " ++ (show y) ++ ") already filled"
          playGame b


main :: IO ()
main = 
 -- haskellSay "Hello, Haskell! You're using a function from another package!"

main = do

--  putStrLn "Number of the game to play (1-9): "
--  s <- getLine
--  let sudoku = read s
--  putStrLn $ "Playing game number" ++ (show sudoku)

  game <- readFile

  let board = read game

  playGame board

