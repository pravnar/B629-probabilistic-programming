module Actions ( Action (..)
               , skip
               , end_skip
               , batch
               , pack
               , batch_print
               , seeMH
               , seeSA
               , print_rem
               ) where

import Control.Monad
import Kernels

type Act x m a = x -> a -> m a

data Action x m a = Action (Act x m a) a

execute :: Monad m => Action x m a -> x -> m (Action x m a)
execute (Action act a) x = act x a >>= return . Action act

skip :: Monad m => Int -> Action x m a -> m (Action x m (a,Int))
skip n (Action act a) = do
  let skip_act x (b,i) = if i == n 
                         then do b' <- act x b
                                 return (b',0)
                         else return (b,i+1)
  return (Action skip_act (a,0))

end_skip :: (a,Int) -> a
end_skip = fst

-- Batch actions --  

type Batch x = ([x], Int)
type BatchAct x m = Act x m (Batch x)
type BatchAction x m = Action x m (Batch x)

batch :: Monad m => ([x] -> m s) -> Int -> BatchAct x m
batch f n x (l, i) 
    | i+1 == n = f (x:l) >> return ([], 0)
    | otherwise = return (x:l, i+1)

pack :: BatchAct x m -> BatchAction x m
pack = flip Action ([], 0)

-- Visualization --

type PrintF x = [x] -> [Double]

viz_json :: [Double] -> String
viz_json samplelist = "{\"rvars\": {\"x\": " ++ show samplelist ++ "}}"

visualize :: PrintF x -> [x] -> IO ()
visualize f ls = unless (null ls) $ putStrLn . viz_json $ f ls

batch_print :: PrintF x -> Int -> BatchAction x IO
batch_print f = pack . batch (visualize f)

print_rem :: PrintF x -> Batch x -> IO ()
print_rem f (xs, _) = visualize f xs

-- MH

seeMH :: PrintF Double
seeMH = id

-- SA

first :: (a, b, c) -> a
first (a,_,_) = a

my_filter :: [Double] -> [Double]
my_filter = filter (((>) 40) . abs)

seeSA :: PrintF (St_SA Double)
seeSA = my_filter . map first
