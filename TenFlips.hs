module TenFlips where

import PM
import Control.Monad

myflip :: Exp Bool
myflip = choose 0.5 (return True) (return False)

tenflips :: Exp Bool
tenflips = do
  fs <- forM [1..10] (const myflip)
  return $ and fs

ans :: Double
ans = expectation
      (\b -> if b then 1.0 else 0.0)
      tenflips

main = print ans