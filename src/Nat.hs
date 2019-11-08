module Nat where


-- data Nat = Zero | Succ Nat

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat


plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ m) n = Succ (m `plus` n)

toNum :: Num n => Nat -> n
toNum = go 0
  where
    go a Zero = a
    go a (Succ m) = go (a + 1) m

fromIntegral :: Integral i => i -> Maybe Nat
fromIntegral i
  | i < 0 = Nothing
  | otherwise = Just $ go Zero i
    where
      go a 0 = a
      go a k = go (Succ a) (k - 1)

instance Show Nat where
  show n = "Nat(" <> show (toNum n :: Integer) <> ")"
