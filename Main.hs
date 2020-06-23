import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char
import Data.List
import qualified MLData as L
import qualified Perceptron as P
import System.Environment
import System.Directory
import System.IO
import qualified Text.Read as T

-- dispatch :: [(String, ([Maybe B.ByteString] -> [String] -> IO (), Int))]
dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("train", train)
           , ("test", test)
           , ("predict", predict)
           ]

runProg :: Maybe ([String] -> IO ()) -> [String] -> IO ()
runProg Nothing _ = putStrLn "Please pass a valid run mode."
runProg (Just act) args = do
    -- let getContents fn
    --         | fnExists  = Just fContents
    --         | otherwise = Nothing
    --         where fnExists <- doesFileExist fn
    --               fContents <- B.readFile fn
    -- let argsSplit = splitAt n_args args
    -- let openFiles = map getContents $ fst argsSplit
    -- act openFiles $ snd argsSplit
    act args

-- What to do about getting input in this function?
-- Add input parsing to main?
-- have runProg return the number of files that it needs?
-- loadLabeled :: String -> Maybe L.ExampleSet
-- loadLabeled fn
--     | fnExists = L.loadExampleSet fContents
--     | otherwise = Nothing
--     where fnExists <- doesFileExist fn
--           fContents <- B.readFile fn

safeOpenFile :: String -> IO (Maybe C.ByteString)
safeOpenFile fn = do
    fnExists <- doesFileExist fn
    if fnExists
        then do fContents <- C.readFile fn
                return (Just fContents)
        else do return Nothing

saveModel :: P.Perceptron -> String -> IO ()
saveModel percep fnOut = do
    -- let labWts = zipWith (\ x y -> (show x) ++ ":" ++ (show y)) [1..] percep
    writeFile fnOut . intercalate "," $ map show percep

test :: [String] -> IO ()
test (dataFn:modelFn:_) = do
    testData <- safeOpenFile dataFn
    modelData <- safeOpenFile modelFn
    let loadAndTest Nothing _ = putStrLn "Data file does not exist."
    let loadAndTest _ Nothing = putStrLn "Model file does not exist."
    let loadAndTest (Just td) (Just md) = do
        let exs = L.loadExampleSet td
        let percep = L.loadWeights md
        putStrLn $ "Testing accuracy: " ++ (show $ P.test percep exs)
    loadAndTest testData modelData
test _ = putStrLn "Not enough arguments passed."

train :: [String] -> IO ()
train (featuresFn:outFn:learningRate:nIters:_) = do
    trainingData <- safeOpenFile featuresFn
    let learningRateNum = T.readMaybe learningRate :: Maybe Float
    let nIterNum = T.readMaybe nIters :: Maybe Int
    let loadAndTrain Nothing _ _ = putStrLn "File does not exist."
    let loadAndTrain _ Nothing _ = putStrLn "Learning rate not a Float."
    let loadAndTrain _ _ Nothing = putStrLn "Number of iterations not an Int."
    let loadAndTrain (Just fContents) (Just lr) (Just i) = do
        let exs = L.loadExampleSet fContents
        let nFeatures = L.findNFeatures exs
        let p0 = P.initPerceptron nFeatures
        let percep = P.train p0 exs lr i
        saveModel percep outFn
    loadAndTrain trainingData learningRateNum nIterNum

train _ = putStrLn "Not enough arguments passed."

predict :: [String] -> IO ()
predict _ = putStrLn "predict"

main = do
    (command:args) <- getArgs
    let action = lookup (map toLower command) dispatch
    runProg action args
    -- input <- B.readFile "data/easy.train"
    -- let ex = L.loadExampleSet input
    -- putStrLn . show . L.findNFeatures $ ex
    -- putStrLn . show . L.exampleToVector (L.findNFeatures ex) $ ex !! 0
    -- let dataArr = map (map C.unpack) . map (C.split ',') $ C.lines test
    -- let dataArrNum = map (map read) dataArr :: [[Float]]
    -- let dataSums = map sum dataArrNum
    -- mapM_ putStrLn $ map show dataSums
    -- mapM_ C.putStrLn $ map C.unwords dataArr
