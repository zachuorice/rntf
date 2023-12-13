module Main where

import System.Environment
import System.Exit
import System.Random
import Data.Time.LocalTime
import Data.Time.Format.ISO8601
import Data.Time.Clock.System

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

usage        = putStrLn "Usage: rntf [-vh] [start end [dir ...],]"
version      = putStrLn "rntf 0.1"
exit         = exitWith   ExitSuccess
failed       = exitWith $ ExitFailure 1

consume ("-r":s:e:fs)   = do
    consumeFiles 0 (parseArgumentGroup True s e) fs

consume (s:e:fs)        = do
    consumeFiles 0 (parseArgumentGroup False s e) fs
consume []              = exit

-- TODO: Directory lister and recursive walker to use consumeFiles

consumeFiles i _ (",":fs)           = consume fs
consumeFiles i _ []                 = consume []
consumeFiles i Nothing _            = failed
consumeFiles i (Just args) (f:fs) = do
    isTime <- checkTime (Just args)
    thePick <- randomPick (i, (i + (length fs))) i
    if isTime && thePick then 
        putStrLn f
    else
        consumeFiles (i+1) (Just args) fs

checkRandomPick (a, stdGen) i = a == i

randomPick r i = do
    time <- getSystemTime
    let seconds = systemSeconds time
    let pureGen = mkStdGen $ fromIntegral seconds
    let result =  uniformR r pureGen
    return $ checkRandomPick result i

checkTime (Just (Arguments s e r)) = do
    zonedTime <- getZonedTime
    let localTime = localTimeOfDay $ zonedTimeToLocalTime zonedTime
    return $ localTime >= s && localTime <= e

