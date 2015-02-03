import AI.GeneticAlgorithm.Simple
import Text.Printf
import Data.List as L
import Control.DeepSeq
import Control.Monad.Random
import Control.Monad

newtype SinInt = SinInt [Double]

instance NFData SinInt where
    rnf (SinInt xs) = rnf xs `seq` ()

instance Show SinInt where
    show (SinInt []) = "<empty SinInt>"
    show (SinInt (x:xs)) =
        let start = printf "%.5f" x
            end = concat $ zipWith (\c p -> printf "%+.5f" c ++ "X^" ++ show p) xs [1 :: Int ..]
        in start ++ end

polynomialOrder = 4 :: Int

err :: SinInt -> Double
err (SinInt xs) =
    let f x = snd $ L.foldl' (\(mlt,s) coeff -> (mlt*x, s + coeff*mlt)) (1,0) xs
    in maximum [ abs $ sin x - f x | x <- [0.0,0.001 .. pi/2]]

instance Chromosome SinInt where
    crossover (SinInt xs) (SinInt ys) =
        return [ SinInt (L.zipWith (\x y -> (x+y)/2) xs ys) ]

    mutation (SinInt xs) = do
        idx <- getRandomR (0, length xs - 1)
        dx  <- getRandomR (-10.0, 10.0)
        let t = xs !! idx
            xs' = take idx xs ++ [t + t*dx] ++ drop (idx+1) xs
        return $ SinInt xs'

    fitness int =
        let max_err = 1000.0 in
        max_err - (min (err int) max_err)

randomSinInt = do
    lst <- replicateM polynomialOrder (getRandomR (-10.0,10.0))
    return $ SinInt lst

stopf :: SinInt -> Int -> IO Bool
stopf best gnum = do
    let e = err best
    _ <- printf "Generation: %02d, Error: %.8f\n" gnum e
    return $ e < 0.0002 || gnum > 20

main = do
    int <- runGAIO 64 0.1 randomSinInt stopf
    putStrLn ""
    putStrLn $ "Result: " ++ show int
