module Web.Api
  ( PingApi

  , User (..)
  , UserApi

  , Api
  ) where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Servant.API


type PingApi = Get '[PlainText] String


data User = User
  { name :: String
  , surname :: String
  }
  deriving Generic

instance FromJSON User

type UserApi
   =  Capture "name" String :> Get '[PlainText] String
 :<|> ReqBody '[JSON] User :> Post '[PlainText] String

type Api
    =  "ping" :> PingApi
  :<|> "greet" :> UserApi
