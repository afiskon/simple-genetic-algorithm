import           AI.GeneticAlgorithm.Simple
import           Control.DeepSeq
import           Data.List                  as L
import           System.Random
import           Text.Printf

newtype SinInt = SinInt [Double]

instance NFData SinInt where
    rnf (SinInt xs) = rnf xs `seq` ()

instance Show SinInt where
    show (SinInt []) = "<empty SinInt>"
    show (SinInt (x:xs)) =
        let start = printf "%.5f" x
            end = concat $ zipWith (\c p -> printf "%+.5f" c ++ "X^" ++ show p) xs [1 :: Int ..]
        in start ++ end

polynomialOrder :: Int
polynomialOrder = 4

err :: SinInt -> Double
err (SinInt xs) =
    let f x = snd $ L.foldl' (\(mlt,s) coeff -> (mlt*x, s + coeff*mlt)) (1,0) xs
    in maximum [ abs $ sin x - f x | x <- [0.0,0.001 .. pi/2]]

instance Chromosome SinInt where
    crossover g (SinInt xs) (SinInt ys) =
        ( [ SinInt (L.zipWith (\x y -> (x+y)/2) xs ys) ], g)

    mutation g (SinInt xs) =
        let (idx, g') = randomR (0, length xs - 1) g
            (dx, g'') = randomR (-10.0, 10.0) g'
            t = xs !! idx
            xs' = take idx xs ++ [t + t*dx] ++ drop (idx+1) xs
        in (SinInt xs', g'')

    fitness int =
        let max_err = 1000.0 in
        max_err - min (err int) max_err

randomSinInt :: RandomGen g => g -> (SinInt, g)
randomSinInt gen =
    let (lst, gen') =
            L.foldl'
                (\(xs, g) _ -> let (x, g') = randomR (-10.0,10.0) g in (x:xs,g') )
                ([], gen) [0..polynomialOrder]
    in (SinInt lst, gen')

stopf :: SinInt -> Int -> IO Bool
stopf best gnum = do
    let e = err best
    _ <- printf "Generation: %02d, Error: %.8f\n" gnum e
    return $ e < 0.0002 || gnum > 20

main :: IO ()
main = do
    int <- runGAIO 64 0.1 randomSinInt stopf
    putStrLn ""
    putStrLn $ "Result: " ++ show int
