import System.Random
import Data.Char as C
import Data.List as L

main :: IO ()
main = undefined

type Weight = Double

-- data Network a b = Input (a -> Weight)
--                 | Neuron Weight [(Network a b, Weight)]
--                 | Output (Weight -> b) (Network a b)

data Neuron = Neuron Weight [(Neuron, Weight)] deriving (Show)

data Network a b = Network [a -> Weight] [Neuron] ([Weight] -> b)

-- fun1 = average . map (fromIntegral . length) . words
-- fun2 = (\str -> foldl (\acc c -> acc + (fromIntegral (ord c) / 122)) 0 str)
-- fun3 = (\str -> (foldl (\acc c -> acc + if c `elem` "aoeui" then 1 else 0) 0 str) / fromIntegral (length str) )

-- neuronB1 = Neuron 0.5 [(neuronA1, 0.5), (neuronA2, 0.5), (neuronA3, 0.5)]
-- neuronB2 = Neuron 0.5 [(neuronA1, 0.5), (neuronA2, 0.5), (neuronA3, 0.5)]
-- neuronB3 = Neuron 0.5 [(neuronA1, 0.5), (neuronA2, 0.5), (neuronA3, 0.5)]
-- neuronB4 = Neuron 0.5 [(neuronA1, 0.5), (neuronA2, 0.5), (neuronA3, 0.5)]

-- neuronC1 = Neuron 0.5 [(neuronB1, 0.5), (neuronB2, 0.5), (neuronB3, 0.5), (neuronB4, 0.5)]
-- neuronC2 = Neuron 0.5 [(neuronB1, 0.5), (neuronB2, 0.5), (neuronB3, 0.5), (neuronB4, 0.5)]


-- Maybe inline this?
sigmoid :: Floating a => a -> a
sigmoid t = 1 / (1 + exp (-t))

diomgis t = log((-t)/(t - 1))

-- length xs > 0
average :: (Fractional a, Foldable t) => t a -> a
average xs = sum xs / (fromIntegral (length xs))

update :: Neuron -> Weight -> Weight
update (Neuron bias neurons) a = sigmoid $ weightedSum / (fromIntegral (length neurons))
    where
        weightedSum = foldl (\acc (neuron, weight) -> acc + update neuron a * weight) (-bias) neurons

for :: [a] -> (a -> b) -> [b]
for = flip map

-- runNetwork :: Network a b -> a -> b
-- runNetwork (Network inputReaders neurons outputWriter ) input = outputWriter $
--     for inputReaders (\inputReader ->
--         for neurons (\neuron ->
--             update neuron (inputReader input)
--         )
--     )

-- trainNeuron :: Weight -> a -> Network a b -> Network a b
-- trainNeuron expected input (Neuron bias neurons) = (Neuron bias newNeurons)
--     where
--         newNeurons = map (\(network, weight) -> let result = update network input in (network, newWeight weight expected result)) neurons
--         newWeight w e r = w * (0.5 + diomgis (abs (e - r)))
-- trainNeuron _ _ n = n

-- train :: a -> b -> (b -> b -> Weight) -> Network a b -> Network a b
-- train input expected scoring network = undefined
--     where result = runNetwork network input
--           score  = scoring result expected
