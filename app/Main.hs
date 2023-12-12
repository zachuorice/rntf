module Main where

import System.Environment
import System.Exit
import Data.Time.LocalTime

main :: IO ()
main = getArgs >>= parse >>= putStr . rntf

data ArgumentGroup = Arguments {
     start   :: TimeOfDay
    ,end     :: TimeOfDay
    ,recurse :: Bool
    ,files   :: [String]
}

rntf files  = "notrandom.jpg"

parse ["-h"] = usage   >> exit 
parse ["-v"] = version >> exit 
parse [fs]   = putStrLn fs
parse []     = exit

usage        = putStrLn "Usage: rntf [-vh] [start end [file ...],]"
version      = putStrLn "rntf 0.1"
exit         = exitWith   ExitSuccess
die          = exitWith $ ExitFailure 1
