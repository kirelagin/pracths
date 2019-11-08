module Vec where

import Data.Kind (Type)

import Nat


data Vec (n :: Nat) (a :: Type) where
  VNil :: Vec 'Zero a
  VCons :: a -> Vec m a -> Vec ('Succ m) a

vhead :: Vec ('Succ n) a -> a
vhead (VCons x _) = x


type family Plus (m :: Nat) (n :: Nat) :: Nat where
  Plus 'Zero n = n
  Plus ('Succ m) n = 'Succ (Plus m n)
--  Plus ('Succ m) n = Plus m ('Succ n)

vappend :: Vec m a -> Vec n a -> Vec (Plus m n) a
vappend VNil ys = ys
vappend (VCons x xs) ys = VCons x (xs `vappend` ys)
--vappend (VCons x xs) ys = xs `vappend` (VCons x ys)

data Proxy (n :: Nat) = Proxy

instance (KnownNat n, Show a) => Show (Vec n a) where
  show v = "Vec{" <> show (natToInteger @n) <> "}[" <> go v
    where
      go :: Vec m a -> String
      go VNil = "]"
      go (VCons x VNil) = show x <> "]"
      go (VCons x xs) = show x <> "," <> go xs

class KnownNat (n :: Nat) where
  natToInteger :: Integer

instance KnownNat 'Zero where
  natToInteger = 0

instance KnownNat n => KnownNat ('Succ n) where
  natToInteger = 1 + natToInteger @n
