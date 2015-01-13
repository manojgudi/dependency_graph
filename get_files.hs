{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad

import qualified Data.Map as M
import Data.String.Utils (replace, strip)
import Data.List (isInfixOf)

import HSH (run)
import System.Directory (doesFileExist)
import System.IO


type NodeNumber       = Int
type CompleteFilePath = String
type LookupFNumber    = M.Map NodeNumber CompleteFilePath
type LookupFPath      = M.Map CompleteFilePath NodeNumber
type FileNodes        = M.Map CompleteFilePath ([CompleteFilePath])

{- Once you go IO
 - You never go back
 - -}
listPythonFiles :: IO [String]
listPythonFiles = do
    myString <- run $ "find -name \"*.py\" " :: IO String
    return $ lines myString


{- Takes a line and returns True if the line starts with "from" keyword
 - else it returns False
 - -}
isImportLine :: String -> Bool
isImportLine someLine = if "from" == (take 4 $ strip someLine) then True else False

{- moduleTree Function
 - Takes pythonFiles [String] a dictionary
 - Populates that dictionary and returns it
 - Using foldl1 means there will be at least 1 python file in the repository
 - -}
moduleTree :: [String] -> IO FileNodes
moduleTree pythonFiles =  myFold getNodes fileNodes pythonFiles
    where fileNodes = M.insert "" [""] M.empty

{- myFold takes a function which operates on file (check getNodes)
 - Takes empty dictionary and populates it 
 - and finally a list of files to be operated on 
 - Returns populated dictionary
 - -} 
myFold ::  (String -> IO [CompleteFilePath]) ->  FileNodes ->  [String] -> IO FileNodes
myFold myFunc dict []     = return dict
myFold myFunc dict (x:xs) = do
    cp <- myFunc x
    val <-  myFold myFunc (M.insert x cp dict) xs
    return val


{- getNodes Takes absolute file path and returns
 -  extracts content from files
 -    extracts lines from content
 -      extracts importLine for each line which is valid
 - Converts importLines to file path
 - Returns file_name : [nodes]
 - -}
getNodes :: String -> IO [CompleteFilePath]
getNodes fileName = do
    fileContent <- readFile fileName
    return [ getFilePath lineContent | lineContent <- lines fileContent, isImportLine lineContent ]

{- Takes line like "from cassandra.cluster import Cluster" and
 - returns "./cassandra/cluster.py"
 -}
getFilePath :: String -> String
getFilePath importLine = "./" ++ (replace "." "/" (words importLine !! 1) ) ++ ".py"



main = do 
    pythonFiles <- listPythonFiles
    out <- moduleTree pythonFiles
    putStrLn $ M.showTree out
