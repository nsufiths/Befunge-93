module Main where

import Lib
import System.IO

main = do
    putStrLn "Type the name of the input file:"
    inputFile <- getLine
    handle <- openFile inputFile ReadMode
    contents <- hGetContents handle
    let linesOfFiles = lines contents
    let newLinesOfFiles = map (\x -> x ++ replicate (80 - length x) ' ') linesOfFiles ++ replicate (25 - length linesOfFiles) (replicate 80 ' ')
    let initialPtr = ProgramPtr { _code = newLinesOfFiles, _stack = [], _direction = (0, 1), _location = (0, 0), _stringMode = False, _currentSymb = ' ', _rnd = 0, _input = [], _output = [] }
    mainCycle initialPtr
    putStr "Program completed successfully!"