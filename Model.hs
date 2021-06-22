import qualified MLData as L

class Model a where
    train :: a -> L.ExampleSet -> Float -> Int -> a
    test :: a -> L.ExampleSet -> Float
    predict :: a -> L.Example -> Maybe Int