-- 
-- rntf: Random timed file selector. (Great for wallpaper scripts!)
-- Copyright (C) 2023 Zachary Tarvid-Richey <zr.public@gmail.com>
-- 
-- 
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-- 

module Main where

import System.Environment
import System.Exit
import System.Random
import System.Directory
import Data.Time.LocalTime
import Data.Time.Format.ISO8601
import Data.Time.Clock.System
import Control.Exception

main :: IO ()
main = getArgs >>= parse

data ArgumentGroup = Arguments {
     start   :: TimeOfDay
    ,end     :: TimeOfDay
}

instance Show ArgumentGroup where
    show (Arguments s e) = do
        show s
        show e

padl :: String -> String -> Int -> String
padl s c l = if l <= 0 then s else c ++ (padl s c (l-1))

parseArgumentGroup :: String -> String -> Maybe ArgumentGroup
parseArgumentGroup start end = do
    startTOD <- parseFormatExtension hourMinuteFormat (padl start "0" (5 - (length start))) -- If input is 8:00 make it 08:00
    endTOD   <- parseFormatExtension hourMinuteFormat (padl end "0" (5 - (length end)))
    Just $ Arguments startTOD endTOD

parse ["-h"]            = usage   >> exit 
parse ["-v"]            = version >> exit 
parse []                = usage   >> failed
parse (_:_:[])          = usage   >> failed
parse fs                = consume fs

usage        = putStrLn "Usage: rntf [-vh] [start end [dir ...],]"
version      = putStrLn "rntf 0.1"
exit         = exitWith   ExitSuccess
failed       = exitWith $ ExitFailure 1

consume :: [String] -> IO()
consume (",":fs) = consume fs
consume (s:e:fs)        = do
    let args = (parseArgumentGroup s e)
    dirs <- consumeDirs (fs, [])
    consumeFilenames 0 args dirs
consume _              = exit

consumeDirs :: ([FilePath], [FilePath]) -> IO([FilePath])
consumeDirs ((p:pl), ol) = do
    spl <- catch (listDirectory p) handler
    consumeDirs (pl, ol++spl)
    where
        handler :: IOError -> IO([FilePath])
        handler _ = do
            consumeDirs (pl, p:ol)
consumeDirs ([],ol) = return ol
consumeDirs _ = do return ([])

consumeFilenames :: Int -> Maybe ArgumentGroup -> [FilePath] -> IO()
consumeFilenames i _ []                 = consume []
consumeFilenames i Nothing _            = usage
consumeFilenames i (Just args) (f:fs) = do
    isTime <- checkTime (Just args)
    thePick <- randomPick (i, (i + (length fs))) i
    if isTime && thePick then 
        putStrLn f
    else
        consumeFilenames (i+1) (Just args) fs

checkRandomPick (a, stdGen) i = a == i

randomPick r i = do
    time <- getSystemTime
    let seconds = systemSeconds time
    let pureGen = mkStdGen $ fromIntegral seconds
    let result =  uniformR r pureGen
    return $ checkRandomPick result i

checkTime (Just (Arguments s e)) = do
    zonedTime <- getZonedTime
    let localTime = localTimeOfDay $ zonedTimeToLocalTime zonedTime
    return $ localTime >= s && localTime <= e

