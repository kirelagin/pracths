{-# OPTIONS_GHC -Wno-partial-fields #-}

module Web.Api.WebHook
  ( WebHookRequest (WebHookRequest)
  , WebHookApi
  ) where

import Servant.API

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)


data WebHookRequest = WebHookRequest
  { text :: String
  }
  deriving (Generic)

instance ToJSON WebHookRequest

type WebHookApi = ReqBody '[JSON] WebHookRequest :> Post '[] NoContent
