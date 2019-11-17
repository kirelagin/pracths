{-# LANGUAGE UndecidableSuperClasses #-}

module DSL.Env
  ( (:::) ((:::))
  , VarTypeMap
  , VarTypeMapCompatible
  , IsDefined
  , HasType

  , Env
  , empty
  , Assign (assign)
  , Lookup (lookup)
  ) where

import Prelude hiding (lookup)

import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (..), TypeError, Symbol)


data a ::: b = a ::: b

type VarTypeMap = [(Symbol ::: Type)]

data Env (map :: VarTypeMap) where
  EnvEmpty :: Env '[]
  EnvCons :: t -> Env map -> Env ((s '::: t) ': map)


empty :: Env '[]
empty = EnvEmpty


-- Assign

class VarTypeMapCompatible s t old
  => Assign
       (s :: Symbol) (t :: Type)
       (old :: VarTypeMap) (new :: VarTypeMap) where
  assign :: t -> Env old -> Env new

instance (VarTypeMapCompatible s t old, new ~ ((s ':::t) ': old))
  => Assign s t old new where
  assign x env  = EnvCons x env


-- HasType, Lookup

type family HasType (s :: Symbol) (t :: Type) (map :: VarTypeMap) :: Constraint where
  HasType s t ((s '::: t) ': _) = ()
  HasType s t ((s '::: t2) ': _) = TypeError (ErrorMismatch s t t2)
  HasType s t ((s2 '::: t2) ': xs) = HasType s t xs
  HasType s _ '[] = TypeError (ErrorUndefined s)

class HasType s t map => Lookup (s :: Symbol) (t :: Type) (map :: VarTypeMap) where
  lookup :: Env map -> t

instance {-# OVERLAPPING #-} t ~ t' => Lookup s t ((s '::: t') ': xs) where
  lookup (EnvCons x _) = x

instance (HasType s t (p ': xs), Lookup s t xs) => Lookup s t (p ': xs) where
  lookup (EnvCons _ xs) = lookup @s @t xs


-- IsDefined

type family IsDefined (s :: Symbol) (map :: VarTypeMap) :: Constraint where
  IsDefined s ((s '::: t) ': _) = ()
  IsDefined s ((s2 '::: t2) ': xs) = IsDefined s xs
  IsDefined s _ = TypeError (ErrorUndefined s)


-- Compatible

type family VarTypeMapLookup (s :: Symbol) (map :: VarTypeMap) :: Maybe Type where
  VarTypeMapLookup _ '[] = 'Nothing
  VarTypeMapLookup s ((s '::: t) ': _) = 'Just t
  VarTypeMapLookup s (_ ': xs) = VarTypeMapLookup s xs

type VarTypeMapCompatible (s :: Symbol) (t :: Type) (map :: VarTypeMap)
  = VarTypeMapCompatible' s t (VarTypeMapLookup s map) ~ 'True

type family VarTypeMapCompatible' (s :: Symbol) (t :: Type) (r :: Maybe Type) :: Bool where
  VarTypeMapCompatible' _ _ 'Nothing = 'True
  VarTypeMapCompatible' _ t ('Just t) = 'True
  VarTypeMapCompatible' s t ('Just o) = TypeError (ErrorMismatch s o t)


--
-- Errors
--

type ErrorMismatch (s :: Symbol) (te :: Type) (ta :: Type) =
      ( 'Text "Variable `" ':<>: 'Text s ':<>: 'Text "`"
  ':$$: 'Text "Expected type: " ':<>: 'ShowType te
  ':$$: 'Text "  Actual type: " ':<>: 'ShowType ta
      )

type ErrorUndefined (s :: Symbol) =
  ('Text "Variable `" ':<>: 'Text s ':<>: 'Text "` is not defined")
