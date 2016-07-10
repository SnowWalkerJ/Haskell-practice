{- Sudoku -}
import Control.Monad

type Sudoku = [[Int]]
valAt::Sudoku->Int->Int->Int
valAt board row col = board!!row!!col
rowAt::Sudoku->Int->Int->[Int]
rowAt board row _ |row<0||row>8 = []
                  |otherwise    = filter (/=0) [valAt board row col| col <-[0..8]]
colAt::Sudoku->Int->Int->[Int]
colAt board _ col |col<0||col>8 = []
                  |otherwise    = filter (/=0) [valAt board row col| row <-[0..8]]
boxAt::Sudoku->Int->Int->[Int]
boxAt board row col = let bigRow = row `div` 3
                          bigCol = col `div` 3
                       in filter (/=0) [valAt board (bigRow*3+i) (bigCol*3+j) | i<-[0..2], j<-[0..2]]
validAt::Sudoku->Int->Int->[Int]
validAt board row col = let r = rowAt board row col
                            c = colAt board row col
                            b = boxAt board row col
                        in [x | x<-[1..9], not (x `elem` (r ++ b ++ c))]

placeAt::Sudoku->Int->Int->Int->Sudoku
placeAt board row col value = [[if x==row && y==col then value else valAt board x y | y<-[0..8]] | x<-[0..8]]

solve::Sudoku->[Sudoku]
solve x = generate 0 0 x
generate::Int->Int->Sudoku->[Sudoku]
generate 9 0 origin = [origin]
generate x y origin | valAt origin x y /= 0 = generate x' y' origin
                    | otherwise             = do
                                                move <- validAt origin x y
                                                generate x' y' (placeAt origin x y move)
                                              where
    x' = if y == 8 then x+1 else x
    y' = if y == 8 then 0 else y+1

printSudoku::Sudoku->IO()
printSudoku game = forM_ [0..8] (\row->forM_ [0..8] (\col->putStr ((show $ valAt game row col) ++ " ")) *> putStrLn "")

readSudoku::String->IO Sudoku
readSudoku filename = do
    txt<-readFile filename
    return (read txt::Sudoku)

main = do 
    game<-readSudoku "sudoku.txt"
    solutions <- return $ solve game
    print $ length solutions
    solution <- return $ head$ solutions
    printSudoku solution

