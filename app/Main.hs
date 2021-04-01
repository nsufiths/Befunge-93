{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lib
import System.Random
import System.IO
import Data.Char
import Data.Sequence
import Data.Foldable
import Data.Array
import Control.Lens
import Data.IORef

up = (-1, 0)
down = (1, 0)
left = (0, -1)
right = (0, 1)

data ProgramPtr = ProgramPtr {
    _code :: [String],
    _myStack :: [Int],
    _direction :: (Int, Int),
    _location :: (Int, Int),
    _currentSymb :: Char,
    _output :: String,
    _rnd :: Int,
    _board :: String,
    _stringMode :: Bool
}
makeLenses ''ProgramPtr

mainCycle :: ProgramPtr -> IO ()
mainCycle prog = do
    let ffProg = saveChar prog
    myRandomNumber <- randomRIO (1, 4) :: IO Int
    let fProg = (set rnd myRandomNumber ffProg)
    if (view currentSymb fProg) == '@' && (view stringMode fProg) == False
        then do outh <- openFile "output.txt" WriteMode
                hPutStrLn outh (view output fProg)
                hClose outh
                return ()
        else do 
                if elem (view currentSymb fProg) ("&~")
                    then do 
                            putStr (view output fProg)
                            cl <- getLine
                            let progAfterInputFromKeyboard = set board cl fProg
                            let progAfterInstruction = makeInstr (view currentSymb progAfterInputFromKeyboard) progAfterInputFromKeyboard
                            let progStatAfterChar = saveChar $ makeStep progAfterInstruction
                            putChar (view currentSymb progAfterInstruction)
                            putChar '\n'
                            putStr $ stackToString (view myStack progAfterInstruction)
                            putChar '\n'
                            putChar '\n'
                            mainCycle progStatAfterChar
                    else do 
                            let progAfterInstruction = makeInstr (view currentSymb fProg) fProg
                            let progStatAfterChar = saveChar $ makeStep progAfterInstruction
                            putChar (view currentSymb progAfterInstruction)
                            putChar '\n'
                            putStr $ stackToString (view myStack progAfterInstruction)
                            putChar '\n'
                            putChar '\n'
                            mainCycle progStatAfterChar

addToStack :: Int -> ProgramPtr -> ProgramPtr
addToStack intElem prog = set myStack (intElem : (_myStack prog)) prog

myTail :: [a] -> [a]
myTail [] = []
myTail list = tail list

popFromStack :: ProgramPtr -> Int
popFromStack prog
    | (view myStack prog) == [] = 0
    | otherwise = head $ view myStack prog

subPopFromStack :: ProgramPtr -> Int
subPopFromStack prog
    | (Data.Foldable.length (view myStack prog)) <= 1 = 0
    | otherwise = (view myStack prog) !! 1

subSubPopFromStack :: ProgramPtr -> Int
subSubPopFromStack prog
    | (Data.Foldable.length (view myStack prog)) <= 2 = 0
    | otherwise = (view myStack prog) !! 2

makeStep :: ProgramPtr -> ProgramPtr
makeStep prog = set location newLocation prog
                    where newLocation = ((posY + dirY) `mod` 25, (posX + dirX) `mod` 80)
                          (posY, posX) = view location prog
                          (dirY, dirX) = view direction prog

saveChar :: ProgramPtr -> ProgramPtr
saveChar prog = set currentSymb ((view code prog) !! posY !! posX) prog
                    where (posY, posX) = view location prog

stackToString :: [Int] -> String
stackToString list = concat $ map show list

makeInstr :: Char -> ProgramPtr -> ProgramPtr
makeInstr ch prog
    | (view stringMode prog) == True && ch /= '"' = set myStack newStack prog
        where newStack = (ord ch) : (view myStack prog)
makeInstr '^' prog = set direction up prog
makeInstr 'v' prog = set direction down prog
makeInstr '<' prog = set direction left prog
makeInstr '>' prog = set direction right prog
makeInstr '_' prog = 
    if (popFromStack prog == 0) || ((view myStack prog) == [])
        then set myStack (myTail (view myStack prog)) . set direction right $ prog
        else set myStack (myTail (view myStack prog)) . set direction left $ prog
makeInstr '|' prog = 
    if (popFromStack prog == 0) || ((view myStack prog) == [])
        then set myStack (myTail (view myStack prog)) . set direction down $ prog
        else set myStack (myTail (view myStack prog)) . set direction up $ prog
makeInstr '#' prog = makeStep prog
makeInstr ':' prog = set myStack newStack prog
    where newStack = (popFromStack prog) : (popFromStack prog) : (myTail (view myStack prog))
makeInstr '\\' prog = set myStack newStack prog
    where newStack = (subPopFromStack prog) : (popFromStack prog) : (Prelude.drop 2 (view myStack prog))
makeInstr '$' prog = set myStack newStack prog
    where newStack = myTail $ view myStack prog
makeInstr 'p' prog = set myStack newStack . set code newCode $ prog
    where newCode = (view code prog) & element posY . element posX .~ (chr asciiCode)
          newStack = Prelude.drop 3 (view myStack prog)
          posY = subPopFromStack prog
          posX = popFromStack prog
          asciiCode = subSubPopFromStack prog
makeInstr 'g' prog = set myStack newStack prog
    where newStack = newPop : (Prelude.drop 2 (view myStack prog))
          posY = subPopFromStack prog
          posX = popFromStack prog
          newPop = ord ((view code prog) !! posY !! posX)
makeInstr '!' prog = set myStack newStack prog
    where newStack
            | popFromStack prog == 0 = 1 : (myTail (view myStack prog))
            | otherwise = 0 : (myTail (view myStack prog))
makeInstr '`' prog = set myStack newStack prog
    where newStack
            | (subPopFromStack prog) > (popFromStack prog) = 1 : (Prelude.drop 2 (view myStack prog))
            | otherwise = 0 : (Prelude.drop 2 (view myStack prog))
makeInstr '.' prog = set myStack newStack . set output newOutput $ prog
    where newOutput = (view output prog) ++ (show (popFromStack prog))
          newStack = myTail $ view myStack prog
makeInstr ',' prog = set myStack newStack . set output newOutput $ prog
    where newOutput = (view output prog) ++ [chr (popFromStack prog)]
          newStack = myTail $ view myStack prog
makeInstr '?' prog = set direction randDirection prog
    where randDirection
            | (view rnd prog) == 1 = (-1,0)
            | (view rnd prog) == 2 = (1,0)
            | (view rnd prog) == 3 = (0,1)
            | otherwise = (0,-1)
makeInstr '&' prog = set myStack newStack prog
    where newStack = (read (view board prog) :: Int) : (view myStack prog)
makeInstr '~' prog = set myStack newStack prog
    where newStack = (ord (head (view board prog))) : (view myStack prog)
makeInstr '"' prog
    | view stringMode prog == False = set stringMode True prog
    | otherwise = set stringMode False prog
makeInstr ch prog
    | elem ch ("0123456789") = set myStack newStack prog
        where newStack = (digitToInt ch) : (view myStack prog)
makeInstr ch prog
    | elem ch ("+-*/%") = set myStack newStack prog
        where newStack = (operation ch subPop pop) : (Prelude.drop 2 (view myStack prog))
              subPop = subPopFromStack prog
              pop = popFromStack prog
              operation ch a b = case ch of '+' -> a + b
                                            '-' -> a - b
                                            '*' -> a * b
                                            '/' -> a `div` b
                                            '%' -> a `mod` b
makeInstr _ prog = prog 

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let linesOfFiles = lines contents
    let newLinesOfFiles = (map (++ (Prelude.replicate 80 ' ')) linesOfFiles) ++ (Prelude.replicate (25 - Data.Foldable.length linesOfFiles) (Prelude.replicate 80 ' '))
    let myPtr = ProgramPtr { _code = newLinesOfFiles, _myStack = [], _direction = (0, 1), _location = (0, 0), _stringMode = False, _currentSymb = ' ', _rnd = 0, _board = [], _output = [] }
    mainCycle myPtr
    putStr "Finished!"