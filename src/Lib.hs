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
    myRandomNumber <- randomRIO (1, 4) :: IO Int
    let progAfterRnd = set rnd myRandomNumber $ saveChar prog
    if view currentSymb progAfterRnd == '@' && view stringMode progAfterRnd == False
        then do outh <- openFile "output.txt" WriteMode
                hPutStrLn outh (view output progAfterRnd)
                hClose outh
                return ()
        else do 
                if elem (view currentSymb progAfterRnd) "&~"
                    then do 
                            putStrLn $ view output progAfterRnd
                            cl <- getLine
                            let progAfterInputFromKeyboard = set board cl progAfterRnd
                            let progStatAfterStep = makeStep $ makeInstr (view currentSymb progAfterInputFromKeyboard) progAfterInputFromKeyboard
                            mainCycle progStatAfterStep
                    else do 
                            let progStatAfterStep = makeStep $ makeInstr (view currentSymb progAfterRnd) progAfterRnd
                            mainCycle progStatAfterStep

myTail :: [a] -> [a]
myTail [] = []
myTail list = tail list

popFromStack :: ProgramPtr -> Int
popFromStack prog
    | view myStack prog == [] = 0
    | otherwise = head $ view myStack prog

subPopFromStack :: ProgramPtr -> Int
subPopFromStack prog
    | length (view myStack prog) <= 1 = 0
    | otherwise = view myStack prog !! 1

subSubPopFromStack :: ProgramPtr -> Int
subSubPopFromStack prog
    | length (view myStack prog) <= 2 = 0
    | otherwise = view myStack prog !! 2

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
    | view stringMode prog == True && ch /= '"' = set myStack newStack prog
        where newStack = (ord ch) : (view myStack prog)
makeInstr '^' prog = set direction up prog
makeInstr 'v' prog = set direction down prog
makeInstr '<' prog = set direction left prog
makeInstr '>' prog = set direction right prog
makeInstr '_' prog = 
    if popFromStack prog == 0 || view myStack prog == []
        then set myStack (myTail (view myStack prog)) . set direction right $ prog
        else set myStack (myTail (view myStack prog)) . set direction left $ prog
makeInstr '|' prog = 
    if popFromStack prog == 0 || view myStack prog == []
        then set myStack (myTail (view myStack prog)) . set direction down $ prog
        else set myStack (myTail (view myStack prog)) . set direction up $ prog
makeInstr '#' prog = makeStep prog
makeInstr ':' prog = set myStack newStack prog
    where newStack = (popFromStack prog) : (popFromStack prog) : (myTail (view myStack prog))
makeInstr '\\' prog = set myStack newStack prog
    where newStack = (subPopFromStack prog) : (popFromStack prog) : (drop 2 (view myStack prog))
makeInstr '$' prog = set myStack newStack prog
    where newStack = myTail $ view myStack prog
makeInstr 'p' prog = set myStack newStack . set code newCode $ prog
    where newCode = (view code prog) & element posY . element posX .~ (chr asciiCode)
          newStack = drop 3 (view myStack prog)
          posY = subPopFromStack prog
          posX = popFromStack prog
          asciiCode = subSubPopFromStack prog
makeInstr 'g' prog = set myStack newStack prog
    where newStack = newPop : (drop 2 (view myStack prog))
          posY = subPopFromStack prog
          posX = popFromStack prog
          newPop = ord (view code prog !! posY !! posX)
makeInstr '!' prog = set myStack newStack prog
    where newStack
            | popFromStack prog == 0 = 1 : (myTail (view myStack prog))
            | otherwise = 0 : (myTail (view myStack prog))
makeInstr '`' prog = set myStack newStack prog
    where newStack
            | subPopFromStack prog > popFromStack prog = 1 : (drop 2 (view myStack prog))
            | otherwise = 0 : (drop 2 (view myStack prog))
makeInstr '.' prog = set myStack newStack . set output newOutput $ prog
    where newOutput = view output prog ++ show (popFromStack prog)
          newStack = myTail $ view myStack prog
makeInstr ',' prog = set myStack newStack . set output newOutput $ prog
    where newOutput = view output prog ++ [chr (popFromStack prog)]
          newStack = myTail $ view myStack prog
makeInstr '?' prog = set direction randDirection prog
    where randDirection
            | view rnd prog == 1 = (-1,0)
            | view rnd prog == 2 = (1,0)
            | view rnd prog == 3 = (0,1)
            | otherwise = (0,-1)
makeInstr '&' prog = set myStack newStack prog
    where newStack = (read (view board prog) :: Int) : (view myStack prog)
makeInstr '~' prog = set myStack newStack prog
    where newStack = (ord (head (view board prog))) : (view myStack prog)
makeInstr '"' prog
    | view stringMode prog == False = set stringMode True prog
    | otherwise = set stringMode False prog
makeInstr ch prog
    | elem ch "0123456789" = set myStack newStack prog
        where newStack = (digitToInt ch) : (view myStack prog)
makeInstr ch prog
    | elem ch "+-*/%" = set myStack newStack prog
        where newStack = (operation ch subPop pop) : (drop 2 (view myStack prog))
              subPop = subPopFromStack prog
              pop = popFromStack prog
              operation ch a b = case ch of '+' -> a + b
                                            '-' -> a - b
                                            '*' -> a * b
                                            '/' -> a `div` b
                                            '%' -> a `mod` b
makeInstr _ prog = prog 