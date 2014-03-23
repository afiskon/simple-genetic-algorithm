-- | Simple parallel genetic algorithm implementation.
module GA.Simple (
    Chromosome(..),
    runGA,
    runGAIO,
    zeroGeneration,
    nextGeneration
  ) where

import System.Random
import qualified Data.List as L
import Control.Parallel.Strategies

-- | Chromosome interface
class NFData a => Chromosome a where
    -- | Crossover function
    crossover :: RandomGen g => g -> a -> a -> ([a],g)
    -- | Mutation function
    mutation :: RandomGen g => g -> a -> (a,g)
    -- | Fitness function. fitness x > fitness y means that x is better than y 
    fitness :: a -> Double

-- | Pure GA implementation
runGA   :: (RandomGen g, Chromosome a)
        => g                        -- ^ Random number generator
        -> Int                      -- ^ Population size
        -> Double                   -- ^ Mutation probability [0, 1]
        -> (g -> (a, g))            -- ^ Random chromosome generator (hint: use closures)
        -> (a -> Int -> Bool)       -- ^ Stopping criteria, 1st arg - best chromosome, 2nd arg - generation number
        -> a                        -- ^ Best chromosome
runGA gen ps mp rnd stopf =
    let (pop, gen') = zeroGeneration gen rnd ps in
    runGA' gen' pop ps mp stopf 0

runGA' gen pop ps mp stopf gnum =
    let best = head pop in
    if stopf best gnum
        then best
        else
            let (pop', gen') = nextGeneration gen pop ps mp in
            runGA' gen' pop' ps mp stopf (gnum+1)

-- | Non-pure GA implementation
runGAIO :: Chromosome a
        => Int                      -- ^ Population size
        -> Double                   -- ^ Mutation probability [0, 1]
        -> (StdGen -> (a, StdGen))  -- ^ Random chromosome generator (hint: use closures)
        -> (a -> Int -> IO Bool)    -- ^ Stopping criteria, 1st arg - best chromosome, 2nd arg - generation number
        -> IO a                     -- ^ Best chromosome
runGAIO ps mp rnd stopf = do
    gen <- newStdGen
    let (pop, gen') = zeroGeneration gen rnd ps
    runGAIO' gen' pop ps mp stopf 0

runGAIO' gen pop ps mp stopf gnum = do
    let best = head pop
    stop <- stopf best gnum
    if stop
        then return best
        else do
            let (pop', gen') = nextGeneration gen pop ps mp
            runGAIO' gen' pop' ps mp stopf (gnum+1)

-- | Generate zero generation. Use this function only if you are going to implement your own runGA.
zeroGeneration  :: (RandomGen g)
                => g                -- ^ Random number generator
                -> (g -> (a, g))    -- ^ Random chromosome generator (hint: use closures)
                -> Int              -- ^ Population size
                -> ([a],g)          -- ^ Zero generation and new RNG
zeroGeneration initGen rnd ps =
    L.foldl'
        (\(xs,gen) _ -> let (c, gen') = rnd gen in ((c:xs),gen'))
        ([], initGen) [1..ps]

-- | Generate next generation (in parallel) using mutation and crossover.
--   Use this function only if you are going to implement your own runGA.
nextGeneration  :: (RandomGen g, Chromosome a)
                => g                -- ^ Random number generator
                -> [a]              -- ^ Current generation
                -> Int              -- ^ Population size
                -> Double           -- ^ Mutation probability
                -> ([a], g)         -- ^ Next generation ordered by fitness (best - first) and new RNG
nextGeneration gen pop ps mp =
    let (gen':gens) = L.unfoldr (Just . split) gen
        chunks = L.zip gens $ init $ L.tails pop
        results = map (\(g, (x:ys)) -> [ (t, fitness t) | t <- nextGeneration' [ (x, y) | y <- ys ] g mp [] ]) chunks
                    `using` parList rdeepseq
        lst = take ps $ L.sortBy (\(_, fx) (_, fy) -> fy `compare` fx) $ concat results
    in ( map fst lst, gen' )

nextGeneration' [] _ _ acc = acc
nextGeneration' ((p1,p2):ps) g0 mp acc =
    let (children0, g1) = crossover g0 p1 p2
        (children1, g2) = L.foldl'
                             (\(xs, g) x -> let (x', g') = mutate g x mp in (x':xs, g'))
                             ([],g1) children0
    in
    nextGeneration' ps g2 mp (children1 ++ acc)

mutate :: (RandomGen g, Chromosome a) => g -> a -> Double -> (a, g)
mutate gen x mp =
    let (r, gen') = randomR (0.0, 1.0) gen in
    if r <= mp  then mutation gen' x
                else (x, gen')
