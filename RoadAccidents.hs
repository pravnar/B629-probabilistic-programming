module RoadAccidents where

import PM

data Action = Stopped | Driving | Braking 
              deriving (Eq, Show)

cautious_driver :: Exp Action
cautious_driver = choose 0.8 (return Stopped) (return Driving)

aggressive_driver :: Exp Action
aggressive_driver = choose 0.3 (return Braking) (return Driving)

crash :: Exp Action -> Exp Action -> Exp Bool
crash d1 d2 = choose 0.9 (both_driving) (return False)
    where both_driving = do
            a1 <- d1
            a2 <- d2
            return $ a1==Driving && a2==Driving

crash_prob :: Double
crash_prob = expectation 
             (\b -> if b then 1.0 else 0.0) 
             (crash aggressive_driver aggressive_driver)

main = print crash_prob