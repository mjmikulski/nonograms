-- (c) Maciej Mikulski
-- Kraków 2017

module Main where

import qualified Data.List as DL
import System.IO  
import System.Environment   

data Field = White | Empty | Black deriving(Show, Eq)

type Row = [Field]
type Controls = [Int]

emptyRow :: Int -> Row
emptyRow n = replicate n Empty

sols :: Int -> Int -> [[Int]]
sols z j = balance z [] (replicate j 0) where
    balance :: Int -> [Int] -> [Int] -> [[Int]]
    balance 0 aku (x:rest) = [aku ++ (x:rest)]
    balance z aku [] = []
    balance z aku (x:rest) = ( balance (z-1) aku ((x+1):rest) ) ++ ( balance z (aku ++ [x]) (rest) )

combine :: Controls -> [Int] -> Row
combine [] [] = []
combine [x] (y1:y2:[])  = (replicate y1 White) ++ (replicate x Black) ++ (replicate y2 White)
combine (x:controls) (y:spaces) = (replicate y White) ++ (replicate x Black) ++ [White] ++ (combine controls spaces) 

countBlacksWithSpaces :: Controls -> Int -- n0 in notes
countBlacksWithSpaces list = sum list + (length list) - 1

coSols :: Int -> Controls  -> [Row] -- znajduje wszystkie rozwiązania dla jednego rzędu
coSols n controls = map (combine controls) (sols z  j ) where
    z = n - (countBlacksWithSpaces controls)
    j = (length controls) + 1

(>>>) :: Field -> Field -> Bool -- do sprawdzania czy ewentualne rozwizanie zgadza sie z już posiadaną wiedzą
(>>>) Black Black = True
(>>>) White White = True
(>>>) Empty _ = True
(>>>) _ _ = False

restrict :: Row -> [Row] -> [Row]
restrict accual proposals = filter (isConsistent accual) proposals where
    isConsistent master slave = DL.and $ map (\(m,s) -> m >>> s) $ zip master slave

infixl 7 *** -- do znajdowania czesci wspólnej rozwiązań
(***) :: Field -> Field -> Field
(***) White White = White
(***) Black Black = Black
(***) _ _         = Empty

isum :: [Row] -> Row
isum list = map (foldr1 (***)) $ (DL.transpose list) 

infixl 6 +++ -- do nakładania nowej wiedzy na plansze
(+++) :: Field -> Field -> Field
--(+++) x Empty = x -- should never happen when using restrict
(+++) Empty x = x
(+++) Black Black = Black
(+++) White White = White

iadd :: Row -> Row -> Row
iadd base new = map (\(b,n)-> b +++ n) (zip base new)

updateRow:: Int -> Controls -> Row -> Row
updateRow n controls row = iadd row $ isum $ restrict row $ coSols n controls

diffs :: Row -> Row -> Int
diffs x y = length $ filter (\(q,p)-> q==p) $ zip x y

data Board = B Int [Controls] [Controls] [Row]
instance Show Board where
    show (B _ _ _ rows) = concat $ map showRow rows where
        showRow :: Row -> String
        showRow row = (map showField row) ++ "\n"
        showField :: Field -> Char
        showField White = '.'
        showField Black = 'X'
        showField Empty = '?'

updateBoard :: Board -> Board
updateBoard board@(B n leftControls upperControls rows) =
    let f = (\(c, r) -> updateRow n c r )
        rows1 = DL.transpose $ map f $ zip leftControls rows
        rows2 = DL.transpose $ map f $ zip upperControls rows1
    in if rows2 == rows then board else updateBoard (B n leftControls upperControls rows2)

main = do  
    (filename:args) <- getArgs
    handle <- openFile (filename ++ ".non") ReadMode 
    --handle <- openFile "clock.non" ReadMode  
    str_n <- hGetLine handle
    hGetLine handle
    str_allControls <- hGetContents handle
    let 
        n = read str_n :: Int
        allControls = map (\x -> read x :: [Int]) $ lines str_allControls
        leftControls = take n allControls
        upperControls = drop (n+1) allControls
        board = B n leftControls upperControls (replicate n (emptyRow n) )
    print n
    if (sum (concat leftControls) /= sum (concat upperControls) ) 
    then putStrLn $ "Controls in file " ++ (filename ++ ".non") ++ " are corrupted!"
    else do
        putStrLn "Controls seem fine. Solving...\n"
        hFlush stdout
        let board_final = updateBoard board
        print board_final
        writeFile (filename ++ ".sol") (show $ board_final)
        hClose handle 
        putStrLn "Done."

