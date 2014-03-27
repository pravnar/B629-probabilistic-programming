import PM

problem = do
  cautious <- choose 0.8 (return False) (return True)
  g <- do
    f <- return (&&)
    x <- return cautious
    f x
  y <- return cautious
  g y

