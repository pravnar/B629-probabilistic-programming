module Actions ( Action (..)
               , execute
               , every
               , Batch
               , BatchAct
               , BatchAction
               , inBatches
               , pack
               , PrintF
               , batchPrint
               ) where

import Control.Monad

type Act x m a = x -> a -> m a

data Action x m a b = Action (Act x m a) (a -> m b) a

execute :: Monad m => Action x m a b -> x -> m (Action x m a b)
execute (Action act fin a) x = liftM (Action act fin) (act x a)

every :: Monad m => Int -> Action x m a b -> Action x m (a,Int) b
every n (Action act fin a) = 
  let skip_act x (b,i) = if i == (n-1)
                         then do b' <- act x b
                                 return (b',0)
                         else return (b,i+1)
      skip_fin = fin . fst
  in Action skip_act skip_fin (a,0)

-- Batch actions --  

type Batch x = ([x], Int)
type BatchAct x m = Act x m (Batch x)
type BatchAction x m b = Action x m (Batch x) b

inBatches :: Monad m => (Batch x -> m b) -> Int -> BatchAct x m
inBatches f n x a@(l, i)
    | i == n = f a >> return ([x], 1)
    | otherwise = return (x:l, i+1)

pack :: (Batch x -> m b) -> BatchAct x m -> BatchAction x m b
pack f act = Action act f ([], 0)

-- Visualization --

type PrintF x s = [x] -> [s]

vizJSON :: Show s => [s] -> String
vizJSON samplelist = "{\"rvars\": {\"x\": " ++ show samplelist ++ "}}"

closeJSON :: Show s => [s] -> String
closeJSON samplelist = "{\"close\": true, \"rvars\": {\"x\": " ++ show samplelist ++ "}}"

visualize :: Show s => PrintF x s -> Batch x -> IO ()
visualize f (ls,_) = unless (null ls) $ putStrLn.vizJSON $ f ls

vizClose :: Show s => PrintF x s -> Batch x -> IO ()
vizClose f (ls,_) = unless (null ls) $ putStrLn.closeJSON $ f ls

batchPrint :: Show s => PrintF x s -> Int -> BatchAction x IO ()
batchPrint f n = 
    let viz = visualize f 
        close = vizClose f
    in pack close $ inBatches viz n

