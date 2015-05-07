-- DSL 2014 Fall Homework 4
-- Zhenfei Nie 
-- NOTE: I deeply disscussed with Xiyu Lyu about this homework.

import Text.ParserCombinators.Parsec
import Control.Applicative((<$>), (<*>), (<*), (*>), (<$))
import Control.Monad.State
import System.Environment (getArgs)
import Debug.Trace (trace)
import Data.Char (isDigit)
import Data.Functor ((<$>))
import Data.List (sort)
import Data.Maybe (isJust, fromJust)
import Numeric (readFloat)

data Process = Entry {
                    entryUser :: String,
                    entryPid :: Int,
                    entryCpu :: Float,
                    entryMemory :: Float,
                    entryVsz :: Int,
                    entryRss :: Int,
                    entryTt :: String,
                    entryStat :: String,
                    entryStarted :: Date,
                    entryTime :: Time,
                    entryCommand :: String
                    } deriving (Show)



--data Date = Date {day :: Int,
--                  month :: Int,
--                  year :: Int} deriving (Show)

type Date = String 

data Time = Time {second :: Int,
                  minute :: Int,
                  hour :: Int} deriving (Show)

parsePs :: String -> [Process]
parsePs s = error "Implement me"


psParser :: GenParser Char st [Process]
psParser = do
    title <- try psTitleLine
    entries <- many psEntry
    return entries

psTitleLine :: GenParser Char st String
psTitleLine = do
    title <- many $ noneOf "\n"
    eol
    return title

psEntry :: GenParser Char st Process
psEntry = do
    spaces
    user <- psUSER
    spaces

    pid <- psPID
    spaces

    cpu <- psCPU
    spaces

    mem <- psMEM
    spaces

    vsz <- psVSZ
    spaces

    rss <- psRSS
    spaces

    tt <- psTT
    spaces

    stat <- psSTAT
    spaces

    started <- psSTARTED
    spaces

    time <- psTIME
    spaces

    command <- psCOMMAND
    eol

    return Entry {
        entryUser = user,
        entryPid = pid,
        entryCpu = cpu,
        entryMemory = mem,
        entryVsz = vsz,
        entryRss = rss,
        entryTt = tt,
        entryStat = stat,
        entryStarted = started,
        entryTime = time,
        entryCommand = command
    }


psUSER :: GenParser Char st String
psUSER = do
    user <- readString
    return user

psPID :: GenParser Char st Int
psPID = do
    pid <- readInt
    return pid
    where readInt = read <$> many digit

psCPU :: GenParser Char st Float
psCPU = do
    num1 <- many $ noneOf "."
    char '.'
    num2 <- many $ noneOf " \t\n"
    let cpu = fst (readFloat (num1 ++ "." ++ num2) !! 0)
    return cpu
    

psMEM :: GenParser Char st Float
psMEM = do
    num1 <- many $ noneOf "."
    char '.'
    num2 <- many $ noneOf " \t\n"
    let mem = fst (readFloat (num1 ++ "." ++ num2) !! 0)
    return mem

psVSZ :: GenParser Char st Int
psVSZ = do 
    vsz <- readInt
    return vsz    
    where readInt = read <$> many digit

psRSS :: GenParser Char st Int
psRSS = do
    rss <- readInt
    return rss
    where readInt = read <$> many digit

psTT :: GenParser Char st String
psTT = do
    tt <- readString
    return tt

psSTAT :: GenParser Char st String
psSTAT = do
    stat <- readString
    return stat


psSTARTED :: GenParser Char st Date
psSTARTED = do
    started <- readString
    return started

psTIME :: GenParser Char st Time
psTIME = do
    sec <- readInt
    char ':'
    mins <- readInt
    char '.'
    hr <- readInt
    return Time {
        second = sec,
        minute = mins,
        hour = hr
    }
    where readInt = read <$> many digit

psCOMMAND :: GenParser Char st String
psCOMMAND = do
    command <- many $ noneOf "\n"
    return command

eol :: GenParser Char st String
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <|> string ""
    <?> "end of line"

-- 

readString = many $ noneOf " \t"

parsePss :: String -> IO ()
parsePss input = putStr input

--main :: IO ()
--main = do
--    contents <- getContents 
--    putStr show 

main = getContents >>= \input -> case parse psParser "<stdin>" input of
  Left err -> print err >> error "Failed."
  Right result -> putStr $ show result
   

































  

