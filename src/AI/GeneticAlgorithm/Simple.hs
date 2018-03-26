{-# LANGUAGE LambdaCase #-}

-- | Simple parallel genetic algorithm implementation.
--
-- > import AI.GeneticAlgorithm.Simple
-- > import System.Random
-- > import Text.Printf
-- > import Data.List as L
-- > import Control.DeepSeq
-- >
-- > newtype SinInt = SinInt [Double]
-- >
-- > instance NFData SinInt where
-- >     rnf (SinInt xs) = rnf xs `seq` ()
-- >
-- > instance Show SinInt where
-- >     show (SinInt []) = "<empty SinInt>"
-- >     show (SinInt (x:xs)) =
-- >         let start = printf "%.5f" x
-- >             end = concat $ zipWith (\c p -> printf "%+.5f" c ++ "X^" ++ show p) xs [1 :: Int ..]
-- >         in start ++ end
-- >
-- > polynomialOrder = 4 :: Int
-- >
-- > err :: SinInt -> Double
-- > err (SinInt xs) =
-- >     let f x = snd $ L.foldl' (\(mlt,s) coeff -> (mlt*x, s + coeff*mlt)) (1,0) xs
-- >     in maximum [ abs $ sin x - f x | x <- [0.0,0.001 .. pi/2]]
-- >
-- > instance Chromosome SinInt where
-- >     crossover g (SinInt xs) (SinInt ys) =
-- >         ( [ SinInt (L.zipWith (\x y -> (x+y)/2) xs ys) ], g)
-- >
-- >     mutation g (SinInt xs) =
-- >         let (idx, g') = randomR (0, length xs - 1) g
-- >             (dx, g'') = randomR (-10.0, 10.0) g'
-- >             t = xs !! idx
-- >             xs' = take idx xs ++ [t + t*dx] ++ drop (idx+1) xs
-- >         in (SinInt xs', g'')
-- >
-- >     fitness int =
-- >         let max_err = 1000.0 in
-- >         max_err - (min (err int) max_err)
-- >
-- > randomSinInt gen =
-- >     let (lst, gen') =
-- >             L.foldl'
-- >                 (\(xs, g) _ -> let (x, g') = randomR (-10.0,10.0) g in (x:xs,g') )
-- >                 ([], gen)
-- >                 [0..polynomialOrder]
-- >     in (SinInt lst, gen')
-- >
-- > stopf :: SinInt -> Int -> IO Bool
-- > stopf best gnum = do
-- >     let e = err best
-- >     _ <- printf "Generation: %02d, Error: %.8f\n" gnum e
-- >     return $ e < 0.0002 || gnum > 20
-- >
-- > main = do
-- >     int <- runGAIO 64 0.1 randomSinInt stopf
-- >     putStrLn ""
-- >     putStrLn $ "Result: " ++ show int

module AI.GeneticAlgorithm.Simple (
    Chromosome(..),
    runGA,
    runGAIO,
    zeroGeneration,
    nextGeneration
  ) where

import           Control.Parallel.Strategies
import qualified Data.List                   as L
import           System.Random

-- | Chromosome interface
class NFData a => Chromosome a where
    -- | Crossover function
    crossover :: RandomGen g => g -> a -> a -> ([a],g)
    -- | Mutation function
    mutation :: RandomGen g => g -> a -> (a,g)
    -- | Fitness function. fitness x > fitness y means that x is better than y
    fitness :: a -> Double

-- | Pure GA implementation.
runGA   :: (RandomGen g, Chromosome a)
        => g                        -- ^ Random number generator
        -> Int                      -- ^ Population size
        -> Double                   -- ^ Mutation probability [0, 1]
        -> (g -> (a, g))            -- ^ Random chromosome generator (hint: use currying or closures)
        -> (a -> Int -> Bool)       -- ^ Stopping criteria, 1st arg - best chromosome, 2nd arg - generation number
        -> a                        -- ^ Best chromosome
runGA gen ps mp rnd stopf =
    let (pop, gen') = zeroGeneration gen rnd ps in
    runGA' gen' pop ps mp stopf 0

runGA'
    :: (Chromosome a, RandomGen g)
    => g -> [a] -> Int -> Double -> (a -> Int -> Bool) -> Int -> a
runGA' gen pop ps mp stopf gnum =
    let best = head pop in
    if stopf best gnum
        then best
        else
            let (pop', gen') = nextGeneration gen pop ps mp in
            runGA' gen' pop' ps mp stopf (gnum+1)

-- | Non-pure GA implementation.
runGAIO :: Chromosome a
        => Int                      -- ^ Population size
        -> Double                   -- ^ Mutation probability [0, 1]
        -> (StdGen -> (a, StdGen))  -- ^ Random chromosome generator (hint: use currying or closures)
        -> (a -> Int -> IO Bool)    -- ^ Stopping criteria, 1st arg - best chromosome, 2nd arg - generation number
        -> IO a                     -- ^ Best chromosome
runGAIO ps mp rnd stopf = do
    gen <- newStdGen
    let (pop, gen') = zeroGeneration gen rnd ps
    runGAIO' gen' pop ps mp stopf 0

runGAIO'
    :: (Chromosome a, RandomGen g, Monad m)
    => g -> [a] -> Int -> Double -> (a -> Int -> m Bool) -> Int -> m a
runGAIO' gen pop ps mp stopf gnum = do
    let best = head pop
    stop <- stopf best gnum
    if stop
        then return best
        else do
            let (pop', gen') = nextGeneration gen pop ps mp
            runGAIO' gen' pop' ps mp stopf (gnum+1)

-- | Generate zero generation. Use this function only if you are going to implement your own runGA.
zeroGeneration  :: g                -- ^ Random number generator
                -> (g -> (a, g))    -- ^ Random chromosome generator (hint: use closures)
                -> Int              -- ^ Population size
                -> ([a],g)          -- ^ Zero generation and new RNG
zeroGeneration initGen rnd ps =
    L.foldl'
        (\(xs, gen) _ -> let (c, gen') = rnd gen in (c:xs, gen'))
        ([], initGen)
        [1..ps]

-- | Generate next generation (in parallel) using mutation and crossover.
--   Use this function only if you are going to implement your own runGA.
nextGeneration  :: (RandomGen g, Chromosome a)
                => g                -- ^ Random number generator
                -> [a]              -- ^ Current generation
                -> Int              -- ^ Population size
                -> Double           -- ^ Mutation probability
                -> ([a], g)         -- ^ Next generation ordered by fitness (best - first) and new RNG
nextGeneration gen pop ps mp =
    let (gen', gens) = case L.unfoldr (Just . split) gen of
            g:gs -> (g, gs)
            []   -> error "empty gens"
        chunks = L.zip gens $ init $ L.tails pop
        results =
            map (\case
                    (g, x:ys) ->
                        [ (t, fitness t)
                        | t <- nextGeneration' [ (x, y) | y <- ys ] g mp []
                        ]
                    (_, []) -> error "empty chunk")
                chunks
            `using` parList rdeepseq
        lst = take ps $ L.sortBy (\(_, fx) (_, fy) -> fy `compare` fx) $ concat results
    in ( map fst lst, gen' )

nextGeneration'
    :: (Chromosome a, RandomGen p) => [(a, a)] -> p -> Double -> [a] -> [a]
nextGeneration' [] _ _ acc = acc
nextGeneration' ((p1,p2):ps) g0 mp acc =
    let (children0, g1) = crossover g0 p1 p2
        (children1, g2) = L.foldl'
                             (\(xs, g) x -> let (x', g') = mutate g x mp in (x':xs, g'))
                             ([], g1)
                             children0
    in
    nextGeneration' ps g2 mp (children1 ++ acc)

mutate :: (RandomGen g, Chromosome a) => g -> a -> Double -> (a, g)
mutate gen x mp =
    let (r, gen') = randomR (0.0, 1.0) gen in
    if r <= mp  then mutation gen' x
                else (x, gen')
