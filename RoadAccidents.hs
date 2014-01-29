module RoadAccidents where

import PM

data Action = Stopped | Driving | Braking 
              deriving (Eq, Show)

-- stopped :: Action -> Double
-- stopped Stopped = 1.0
-- stopped _ = 0.0

-- driving :: Action -> Double
-- driving Driving = 1.0
-- driving _ = 0.0

-- braking :: Action -> Double
-- braking Braking = 1.0
-- braking _ = 0.0

cautious_driver :: Exp Action
cautious_driver = choose 0.8 (return Stopped) (return Driving)

aggressive_driver :: Exp Action
aggressive_driver = choose 0.3 (return Braking) (return Driving)

-- crash :: Action -> Action -> Exp Bool
-- crash d1 d2 = choose 0.9 (return $ d1==Driving && d2==Driving) (return False)

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