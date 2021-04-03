{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( ProgramPtr (..),
      mainCycle,
      myTail,
      popFromStack,
      subPopFromStack,
      subSubPopFromStack,
      makeStep,
      saveChar,
      makeInstr
    ) where

import System.Random (randomRIO)
import System.IO
import Data.Char
import Control.Lens

up = (-1, 0)
down = (1, 0)
left = (0, -1)
right = (0, 1)

data ProgramPtr = ProgramPtr {
    _code :: [String],
    _stack :: [Int],
    _direction :: (Int, Int),
    _location :: (Int, Int),
    _currentSymb :: Char,
    _output :: String,
    _rnd :: Int,
    _input :: String,
    _stringMode :: Bool
}
makeLenses ''ProgramPtr

mainCycle :: ProgramPtr -> IO ()
mainCycle prog = do 
    myRandomNumber <- randomRIO (1, 4) :: IO Int
    let progAfterRnd = set rnd myRandomNumber $ saveChar prog
    let action | view currentSymb progAfterRnd == '@' && view stringMode progAfterRnd == False = do
                    outh <- openFile "output.txt" WriteMode
                    hPutStrLn outh (view output progAfterRnd)
                    hClose outh
                    return ()
                | elem (view currentSymb progAfterRnd) "&~" = do
                    putStrLn $ view output progAfterRnd
                    cl <- getLine
                    let progAfterInput = set input cl progAfterRnd
                    let progStatAfterStep = makeStep $ makeInstr (view currentSymb progAfterInput) progAfterInput
                    mainCycle progStatAfterStep
                | otherwise = do
                    let progStatAfterStep = makeStep $ makeInstr (view currentSymb progAfterRnd) progAfterRnd
                    mainCycle progStatAfterStep
    action

myTail :: [a] -> [a]
myTail [] = []
myTail list = tail list

popFromStack :: ProgramPtr -> Int
popFromStack prog
    | view stack prog == [] = 0
    | otherwise = head $ view stack prog

subPopFromStack :: ProgramPtr -> Int
subPopFromStack prog
    | length (view stack prog) <= 1 = 0
    | otherwise = view stack prog !! 1

subSubPopFromStack :: ProgramPtr -> Int
subSubPopFromStack prog
    | length (view stack prog) <= 2 = 0
    | otherwise = view stack prog !! 2

makeStep :: ProgramPtr -> ProgramPtr
makeStep prog = set location newLocation prog
                    where newLocation = ((posY + dirY) `mod` 25, (posX + dirX) `mod` 80)
                          (posY, posX) = view location prog
                          (dirY, dirX) = view direction prog

saveChar :: ProgramPtr -> ProgramPtr
saveChar prog = set currentSymb (view code prog !! posY !! posX) prog
                    where (posY, posX) = view location prog

makeInstr :: Char -> ProgramPtr -> ProgramPtr
makeInstr ch prog
    | view stringMode prog == True && ch /= '"' = set stack newStack prog
        where newStack = ord ch : view stack prog
makeInstr '^' prog = set direction up prog
makeInstr 'v' prog = set direction down prog
makeInstr '<' prog = set direction left prog
makeInstr '>' prog = set direction right prog
makeInstr '_' prog = 
    if popFromStack prog == 0 || view stack prog == []
        then set stack (myTail (view stack prog)) . set direction right $ prog
        else set stack (myTail (view stack prog)) . set direction left $ prog
makeInstr '|' prog = 
    if popFromStack prog == 0 || view stack prog == []
        then set stack (myTail (view stack prog)) . set direction down $ prog
        else set stack (myTail (view stack prog)) . set direction up $ prog
makeInstr '#' prog = makeStep prog
makeInstr ':' prog = set stack newStack prog
    where newStack = popFromStack prog : popFromStack prog : myTail (view stack prog)
makeInstr '\\' prog = set stack newStack prog
    where newStack = subPopFromStack prog : popFromStack prog : drop 2 (view stack prog)
makeInstr '$' prog = set stack newStack prog
    where newStack = myTail $ view stack prog
makeInstr 'p' prog = set stack newStack . set code newCode $ prog
    where newCode = view code prog & element posY . element posX .~ chr asciiCode
          newStack = drop 3 (view stack prog)
          posY = subPopFromStack prog
          posX = popFromStack prog
          asciiCode = subSubPopFromStack prog
makeInstr 'g' prog = set stack newStack prog
    where newStack = newPop : drop 2 (view stack prog)
          posY = subPopFromStack prog
          posX = popFromStack prog
          newPop = ord (view code prog !! posY !! posX)
makeInstr '!' prog = set stack newStack prog
    where newStack
            | popFromStack prog == 0 = 1 : myTail (view stack prog)
            | otherwise = 0 : myTail (view stack prog)
makeInstr '`' prog = set stack newStack prog
    where newStack
            | subPopFromStack prog > popFromStack prog = 1 : drop 2 (view stack prog)
            | otherwise = 0 : drop 2 (view stack prog)
makeInstr '.' prog = set stack newStack . set output newOutput $ prog
    where newOutput = view output prog ++ show (popFromStack prog)
          newStack = myTail $ view stack prog
makeInstr ',' prog = set stack newStack . set output newOutput $ prog
    where newOutput = view output prog ++ [chr (popFromStack prog)]
          newStack = myTail $ view stack prog
makeInstr '?' prog = set direction randDirection prog
    where randDirection
            | view rnd prog == 1 = up
            | view rnd prog == 2 = down
            | view rnd prog == 3 = right
            | otherwise = left
makeInstr '&' prog = set stack newStack prog
    where newStack = (read (view input prog) :: Int) : view stack prog
makeInstr '~' prog = set stack newStack prog
    where newStack = ord (head (view input prog)) : view stack prog
makeInstr '"' prog
    | view stringMode prog == False = set stringMode True prog
    | otherwise = set stringMode False prog
makeInstr ch prog
    | elem ch "0123456789" = set stack newStack prog
        where newStack = digitToInt ch : view stack prog
makeInstr ch prog
    | elem ch "+-*/%" = set stack newStack prog
        where newStack = operation ch subPop pop : drop 2 (view stack prog)
              subPop = subPopFromStack prog
              pop = popFromStack prog
              operation ch a b = case ch of '+' -> a + b
                                            '-' -> a - b
                                            '*' -> a * b
                                            '/' -> a `div` b
                                            '%' -> a `mod` b
makeInstr _ prog = prog