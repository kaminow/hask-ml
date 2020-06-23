module MLData where

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map as Map

data Example = Example { lab :: Int, features :: FeatureList} deriving (Show)
type FeatureList = Map.Map Int Float
type ExampleSet = [Example]

parseExample :: [C.ByteString] -> Example
parseExample (lab:features) = 
    let parseFeature (f:v:_) = (read f, read v)
        featuresSplit = map (map C.unpack) . map (C.split ':') $ features
        featureTuples = map parseFeature featuresSplit
        featureList = Map.fromList featureTuples
        intLab = read . C.unpack $ lab
    in Example intLab featureList


loadExampleSet :: C.ByteString -> ExampleSet
loadExampleSet input =
    let inputWords = map C.words $ C.lines input
    in map parseExample inputWords

loadWeights :: C.ByteString -> [Float]
loadWeights input = map read . map C.unpack . C.split ',' $ input

exampleToVector :: Int -> Example -> [Float]
exampleToVector n_features (Example _ fl) = 
    let findVal i = case Map.lookup i fl of
            Nothing -> 0
            Just (v) -> v
    in [findVal x | x <- [1..n_features]]

findNFeatures :: ExampleSet -> Int
findNFeatures [] = 0
findNFeatures exs = 
    let maxFeature (Example _ features) = maximum $ Map.keys features
    in maximum . map maxFeature $ exs
