module Generic where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)


class HasName (t :: Type) where
  typeName :: String
  default typeName :: GetMetaName (Rep t) => String
  typeName = getMetaName @(Rep t)

class GetMetaName t where
  getMetaName :: String

instance KnownSymbol n => GetMetaName (M1 D ('MetaData n m p nt) f) where
  getMetaName = symbolVal (Proxy @n)


data CoolType = Cool | Foo | Bar
  deriving (Generic, HasName)
