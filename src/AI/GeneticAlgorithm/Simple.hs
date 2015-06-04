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
-- >    crossover (SinInt xs) (SinInt ys) =
-- >        return [ SinInt (L.zipWith (\x y -> (x+y)/2) xs ys) ]
-- >
-- >    mutation (SinInt xs) = do
-- >        idx <- getRandomR (0, length xs - 1)
-- >        dx  <- getRandomR (-10.0, 10.0)
-- >        let t = xs !! idx
-- >            xs' = take idx xs ++ [t + t*dx] ++ drop (idx+1) xs
-- >        return $ SinInt xs'
-- >
-- >     fitness int =
-- >         let max_err = 1000.0 in
-- >         max_err - (min (err int) max_err)
-- >
-- > randomSinInt gen =
-- >     lst <- replicateM polynomialOrder (getRandomR (-10.0,10.0))
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
    nextGeneration, roulette
  ) where

import System.Random
import qualified Data.List as L
import Control.Parallel.Strategies

import Control.Monad.Random
import Control.Monad
import Control.Monad.IO.Class (liftIO)


-- | Chromosome interface
class NFData a => Chromosome a where
    -- | Crossover function
    crossover :: RandomGen g => a -> a -> Rand g [a]
    -- | Mutation function
    mutation :: RandomGen g => a -> Rand g a
    -- | Fitness function. fitness x > fitness y means that x is better than y
    fitness :: a -> Double



-- | Pure GA implementation.
runGA   :: (RandomGen g, Chromosome a)
        => g                        -- ^ Random number generator
        -> Int                      -- ^ Population size
        -> Double                   -- ^ Mutation probability [0, 1]
        -> Rand g a                 -- ^ Random chromosome generator (hint: use currying or closures)
        -> (a -> Int -> Bool)       -- ^ Stopping criteria, 1st arg - best chromosome, 2nd arg - generation number
        -> a                        -- ^ Best chromosome
runGA gen ps mp rnd stopf = evalRand go gen
  where
    go = do
      pop <- zeroGeneration rnd ps
      runGA' pop ps mp stopf 0

runGA' pop ps mp stopf gnum = do
    let best = head pop
    if stopf best gnum
        then return best
        else do
            pop' <- nextGeneration pop ps mp
            runGA' pop' ps mp stopf (gnum+1)

-- | Non-pure GA implementation.
runGAIO :: Chromosome a
        => Int                      -- ^ Population size
        -> Double                   -- ^ Mutation probability [0, 1]
        -> RandT StdGen IO a        -- ^ Random chromosome generator (hint: use currying or closures)
        -> (a -> Int -> IO Bool)    -- ^ Stopping criteria, 1st arg - best chromosome, 2nd arg - generation number
        -> IO a                     -- ^ Best chromosome
runGAIO ps mp rnd stopf = do
  gen <- getStdGen
  evalRandT go gen
  where
    go = do
      pop <- zeroGeneration rnd ps
      runGAIO' pop ps mp stopf 0

runGAIO' :: (RandomGen g, Chromosome a) => [a]  -> Int -> Double -> (a -> Int -> IO Bool) -> Int -> RandT g IO a
runGAIO' pop ps mp stopf gnum = do
    let best = head pop
    stop <- liftIO $ stopf best gnum
    if stop
        then return best
        else do
            pop' <- nextGeneration pop ps mp
            runGAIO' pop' ps mp stopf (gnum+1)

-- | Generate zero generation. Use this function only if you are going to implement your own runGA.
zeroGeneration  :: (Monad m,RandomGen g)
                => RandT g m a         -- ^ Random chromosome generator (hint: use closures)
                -> Int                 -- ^ Population size
                -> RandT g m [a]       -- ^ Zero generation
zeroGeneration rnd ps =
    replicateM ps rnd

-- | Generate next generation (in parallel) using mutation and crossover.
--   Use this function only if you are going to implement your own runGA.
nextGeneration  :: (Monad m,RandomGen g, Chromosome a)
                => [a]              -- ^ Current generation
                -> Int              -- ^ Population size
                -> Double           -- ^ Mutation probability
                -> RandT g m [a]    -- ^ Next generation ordered by fitness (best - first)
nextGeneration pop ps mp = do
    gen <- getSplit
    let gens = L.unfoldr (Just . split) gen
        chunks = L.zip gens $ init $ L.tails pop
        results = map (\(g, x : ys) -> [ (t,fitness t) | t <- evalRand (nextGeneration' [ (x, y) | y <- ys ] mp []) g ]) chunks
                    `using` parList rdeepseq
    -- r <- roulette ps $ normalize $ concat results
    -- return $ map fst $ L.sortBy (\(_, fx) (_, fy) -> fy `compare` fx) r
    let lst = take ps $ L.sortBy (\(_, fx) (_, fy) -> fy `compare` fx) $ concat results
    return $ map fst lst

nextGeneration' [] _ acc = return acc
nextGeneration' ((p1,p2):ps) mp acc = do
    children0 <- crossover p1 p2
    children1 <- mapM (`mutate` mp) children0
    nextGeneration' ps mp (children1 ++ acc)

mutate :: (RandomGen g, Chromosome a) => a -> Double -> Rand g a
mutate x mp = do
    r <- getRandomR (0.0, 1.0)
    if r <= mp  then mutation x
                else return x

normalize :: [(a,Double)] -> [(a,Double)]
normalize xs =
    let
        ws = map snd xs
        mi = minimum ws
        -- mx = maximum ws
        -- df = mx-mi
        df = if mi < 0 then (-mi) else 0
        --(w-mi)/df)
    in map (\(a,w)->(a,(w+df)**4)) xs

roulette :: (MonadRandom m) =>  Int -> [(a,Double)] -> m [(a,Double)]
roulette _ [] = error "roulette called with empty list"
roulette _ [a] = return [a]
roulette l xs = do
  snd <$> foldM go (xs,[]) [1..l]
  where
    go (xs0,xsaccum) _ = do
      (xs1,xs2) <- fromList' xs0
      return (xs2,xs1:xsaccum)
--  let s = (sum (map snd xs)) -- total weight
--      cs = scanl1 (\(_,q) (y,s') -> (y, s'+q)) xs       -- cumulative weight
--  sequence $ replicate l (select1 s cs)
--  where
--    select1 s cs= do
--        p <- getRandomR (0.0,s)
--        let l1 = takeWhile (\(_,q) -> q < p) cs
--        let (r:l2) = drop (length l1) cs
--        return (r,l1++l2)

fromList' :: (MonadRandom m) => [(a,Double)] -> m ((a,Double),[(a,Double)])
fromList' [] = error "fromList' called with empty list"
fromList' [a] = return (a,[])
fromList' xs = do
  let s = sum (map snd xs) -- total weight
      xs2 = map (\(a,b)->(a,b,b)) xs
      cs = scanl1 (\(_,_,q) (y,b,s') -> (y, b,s'+q)) xs2       -- cumulative weight
  p <- getRandomR (0.0,s)
  let l1 = takeWhile (\(_,_,q) -> q < p) cs
  let (r:l2) = drop (length l1) cs
  return (fst' r,map fst' $ l1++l2)
  where fst' (a,b,_)=(a,b)
