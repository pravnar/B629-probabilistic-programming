module Actions ( Action (..)
               , execute
               , skip
               , endSkip
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

skip :: Monad m => Int -> Action x m a b -> Action x m (a,Int) b
skip n (Action act fin a) = 
  let skip_act x (b,i) = if i == (n-1)
                         then do b' <- act x b
                                 return (b',0)
                         else return (b,i+1)
      skip_fin = fin . fst
  in Action skip_act skip_fin (a,0)

endSkip :: Monad m => (a,Int) -> m a
endSkip = return . fst

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

type PrintF x = [x] -> [Double]

vizJSON :: [Double] -> String
vizJSON samplelist = "{\"rvars\": {\"x\": " ++ show samplelist ++ "}}"

closeJSON :: [Double] -> String
closeJSON samplelist = "{\"close\": true, \"rvars\": {\"x\": " ++ show samplelist ++ "}}"

visualize :: PrintF x -> Batch x -> IO ()
visualize f (ls,_) = unless (null ls) $ putStrLn.vizJSON $ f ls

vizClose :: PrintF x -> Batch x -> IO ()
vizClose f (ls,_) = unless (null ls) $ putStrLn.closeJSON $ f ls

batchPrint :: PrintF x -> Int -> BatchAction x IO ()
batchPrint f n = 
    let viz = visualize f 
        close = vizClose f
    in pack close $ inBatches viz n

