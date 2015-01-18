{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

import Control.Arrow(second)
import Data.GraphViz 
import qualified Data.Map as M

import Data.GraphViz hiding (graphToDot)
import Data.GraphViz.Attributes.Complete( Attribute(RankDir, Splines, FontName)
                                        , RankDir(FromLeft), EdgeType(SplineEdges))
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

import GetFileMap (listPythonFiles, moduleTree, CompleteFilePath, FileNodes)


type NodeNumber       = Int
type LookupNodeNumber = M.Map NodeNumber CompleteFilePath
type LookupFilePath   = M.Map CompleteFilePath NodeNumber

{-
 - -}
nodes :: [(Int, [Int], Bool)] -> [(Int, String)]
nodes numberNodes = map (\x -> (x, "")) [1..(length numberNodes)]

graph :: [(Int, [Int], Bool)] -> DotGraph Int
graph numberNodes = graphElemsToDot graphParams (nodes numberNodes) (flattenData numberNodes)

graphParams :: GraphvizParams Int String Bool () String
graphParams = defaultParams { globalAttributes = gStyle }

{-
 - Returns the index of the element in a listOfElements
 - Returns -1 if not found
 - -}
getIndex ::  CompleteFilePath -> [CompleteFilePath] -> Int
getIndex element listOfElements = fromMaybe (-1) (elemIndex element listOfElements)

{- createLookupNodeNumber Creates a dictionary
 - of their elements and their absolute filenames
 - for ex: { 0: "supa.py", 1: "honeycomb/honeycomb.py" }
 - -}
createLookupNodeNumber :: FileNodes -> LookupNodeNumber
createLookupNodeNumber fileNodes = (M.fromList lookupNodeNumber)
    where lookupNodeNumber      = [ (getIndex x (M.keys fileNodes) , x) | x <- M.keys fileNodes ]

{- createLookupFilePath creates an inverse mapping of createLookupNodeNumber
 - -}
createLookupFilePath :: FileNodes -> LookupFilePath
createLookupFilePath fileNodes = (M.fromList lookupFilePath)
    where lookupFilePath = [ (x, getIndex x (M.keys fileNodes) ) | x <- M.keys fileNodes ] 

{-
 - Converts fileNodes mapping to equaivalent number mapping called numberNodes 
 - -}
convertFileNode :: FileNodes -> LookupFilePath -> [(Int, [Int], Bool)]
convertFileNode fileNodes lookupFilePath = [ (fromMaybe (-1) (M.lookup x lookupFilePath), [ fromMaybe (-1) (M.lookup z lookupFilePath ) | z <- y ] , True) | (x,y) <- (M.toList fileNodes) ]

{-
 - Takes numberNodes and flattens the data to resemble one to one mapping
 - -}
flattenData :: [(Int, [Int], Bool)] -> [(Int, Int, Bool)]
flattenData numberNode =  foldl1 (++) [ expandData (fst' x) (snd' x) (thd' x) | x <- numberNode ]

{-
 - Could be made into a lambda function | but this is neater
 - used in flattenData
 - -}
expandData :: Int -> [Int] -> Bool -> [(Int, Int, Bool)]
expandData a [] c     = []
expandData a (x:xs) c = (a, x, c) : (expandData a xs c)


{-
 - Is there a better way to get elements of this 3 element tuple
 - -}
fst' (x, _, _) = x
snd' (_, y, _) = y
thd' (_, _, z) = z

gStyle :: [GlobalAttributes]
gStyle = [ GraphAttrs [RankDir FromLeft, Splines SplineEdges, FontName "courier"]
         , NodeAttrs  [textLabel "\\N", shape PlainText, fontColor Blue]
         , EdgeAttrs  [color Black, style dotted]
        ]

{-
 - The return is useless
 - -}
main :: IO [(Int, [Int], Bool)]
main = do 
    pythonFiles <- listPythonFiles
    fileNodes <- moduleTree pythonFiles
    let numberNodes  = convertFileNode fileNodes $ createLookupFilePath fileNodes
    addExtension (runGraphviz $ graph numberNodes) Png "generated_graph"
    return numberNodes
--main = addExtension (runGraphviz graph) Png "graph"
