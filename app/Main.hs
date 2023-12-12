module Main where

import System.Environment
import System.Exit
import System.Random
import Data.Time.LocalTime
import Data.Time.Format.ISO8601

main :: IO ()
main = getArgs >>= parse

data ArgumentGroup = Arguments {
     start   :: TimeOfDay
    ,end     :: TimeOfDay
    ,recurse :: Bool
}

parseArgumentGroup recurse start end = do
    startTOD <- parseFormatExtension hourMinuteFormat start
    endTOD   <- parseFormatExtension hourMinuteFormat end
    Just $ Arguments startTOD endTOD recurse
printArgumentGroup (Arguments s e r) = do
    print s
    print e
    print r

parse ["-h"]            = usage   >> exit 
parse ["-v"]            = version >> exit 
parse []                = usage   >> failed
parse (_:_:[])          = usage   >> failed
parse fs                = consume fs

usage        = putStrLn "Usage: rntf [-vh] [start end [file ...],]"
version      = putStrLn "rntf 0.1"
exit         = exitWith   ExitSuccess
failed       = exitWith $ ExitFailure 1

consume ("-r":s:e:fs)   = do
    consumeFiles (parseArgumentGroup True s e) fs

consume (s:e:fs)        = do
    consumeFiles (parseArgumentGroup False s e) fs
consume []              = exit

consumeFiles _ (",":fs)  = consume fs
consumeFiles _ []        = consume []
consumeFiles Nothing _   = failed
consumeFiles (Just args) (f:fs) = do
    let (Arguments s e r) = args
    putStrLn f
    printArgumentGroup args
    consumeFiles (Just args) fs

checkTime (Just (Arguments s e r)) = do
    zonedTime <- getZonedTime
    let localTime = localTimeOfDay $ zonedTimeToLocalTime zonedTime
    return $ localTime >= s && localTime <= e

