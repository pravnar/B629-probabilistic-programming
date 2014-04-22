module Actions ( Action (..)
               , execute
               , skip
               , endSkip
               , Batch
               , BatchAct
               , BatchAction
               , batch
               , pack
               , PrintF
               , batchPrint
               , printRem
               ) where

import Control.Monad

type Act x m a = x -> a -> m a

data Action x m a = Action (Act x m a) a

execute :: Monad m => Action x m a -> x -> m (Action x m a)
execute (Action act a) x = liftM (Action act) (act x a)

skip :: Monad m => Int -> Action x m a -> m (Action x m (a,Int))
skip n (Action act a) = do
  let skip_act x (b,i) = if i == n 
                         then do b' <- act x b
                                 return (b',0)
                         else return (b,i+1)
  return (Action skip_act (a,0))

endSkip :: Monad m => (a,Int) -> m a
endSkip = return . fst

-- Batch actions --  

type Batch x m = ([x], Int, [x] -> m ())
type BatchAct x m = Act x m (Batch x m)
type BatchAction x m = Action x m (Batch x m)

batch :: Monad m => Int -> BatchAct x m
batch n x (l, i, f) 
    | i+1 == n = f (x:l) >> return ([], 0, f)
    | otherwise = return (x:l, i+1, f)

pack :: ([x] -> m ()) -> BatchAct x m -> BatchAction x m
pack f = flip Action ([], 0, f)

-- Visualization --

type PrintF x = [x] -> [Double]

vizJSON :: [Double] -> String
vizJSON samplelist = "{\"rvars\": {\"x\": " ++ show samplelist ++ "}}"

visualize :: PrintF x -> [x] -> IO ()
visualize f ls = unless (null ls) $ putStrLn . vizJSON $ f ls

batchPrint :: PrintF x -> Int -> BatchAction x IO
batchPrint f n = pack (visualize f) $ batch n

printRem :: Batch x IO -> IO ()
printRem (xs, _ , f) = f xs
