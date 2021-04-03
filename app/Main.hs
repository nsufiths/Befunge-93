module Main where

import Lib
import System.IO

main = do
    putStrLn "Type the name of the input file:"
    whatToOpen <- getLine
    handle <- openFile (whatToOpen) ReadMode
    contents <- hGetContents handle
    let linesOfFiles = lines contents
    let newLinesOfFiles = map (\x -> x ++ replicate (80 - length x) ' ') linesOfFiles ++ replicate (25 - length linesOfFiles) (replicate 80 ' ')
    let myPtr = ProgramPtr { _code = newLinesOfFiles, _myStack = [], _direction = (0, 1), _location = (0, 0), _stringMode = False, _currentSymb = ' ', _rnd = 0, _board = [], _output = [] }
    mainCycle myPtr
    putStr "Finished!"