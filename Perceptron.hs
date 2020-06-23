module Perceptron where

import qualified Data.Map as Map
import qualified MLData as L

-- data Perceptron = Perceptron { wts :: Weights}
type Perceptron = [Float]


initPerceptron :: Int -> Perceptron
initPerceptron n = replicate n 0

predict :: Perceptron -> L.Example -> Maybe Int
predict [] _ = Nothing
predict percep ex@(L.Example _ fl)
    | Map.null fl = Nothing
    | otherwise = let n_features = length percep
                      fvec = L.exampleToVector n_features ex
                      dotprod = sum $ zipWith (*) fvec percep
    in if dotprod >= 0 then Just 1 else Just 0

shouldUpdateWeights :: Int -> Maybe Int -> Bool
shouldUpdateWeights _ Nothing = False
shouldUpdateWeights trueLab (Just predLab) = not $ trueLab == predLab

updateWeights :: Perceptron -> Float -> L.Example -> Perceptron
updateWeights percep learningRate ex@(L.Example trueLab _) = 
    let updateRate
            | trueLab == 1 = learningRate
            | otherwise    = (-learningRate)
        updateStep = map (*updateRate) $ L.exampleToVector (length percep) ex
    in zipWith (+) percep updateStep

test :: Perceptron -> L.ExampleSet -> Float
test percep exs =
    let isPredRight Nothing _ = 0
        isPredRight (Just predLab) (L.Example trueLab _)
            | predLab == trueLab = 1
            | otherwise          = 0
        predictions = map (predict percep) exs
        whichCorrect = zipWith isPredRight predictions exs
    in (sum whichCorrect) / (fromIntegral (length whichCorrect))

train :: Perceptron -> L.ExampleSet -> Float -> Int -> Perceptron
train percep exs learningRate nIters =
    let shouldUpdate wts ex@(L.Example trueLab _) = 
            shouldUpdateWeights trueLab $ predict wts ex
        newWeights wts ex
            | shouldUpdate wts ex = updateWeights wts learningRate ex
            | otherwise           = wts
        nExs = length exs
        totalExs = take (nExs * nIters) $ cycle exs
    in foldl newWeights percep exs