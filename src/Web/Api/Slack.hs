{-# OPTIONS_GHC -Wno-partial-fields #-}

module Web.Api.Slack where

import Data.Aeson (FromJSON (parseJSON), Options (..), SumEncoding (..), defaultOptions, genericParseJSON, camelTo2)
import GHC.Generics (Generic)
import Servant.API


{-
{
    "token": "Jhj5dZrVaK7ZwHHjRyZWjbDl",
    "challenge": "3eZbrw1aBm2rZgRNFdxV2595E9CY3gmdALWMmHkvFXO7tYXAYM8P",
    "type": "url_verification"
}
```

```
{
    "token": "one-long-verification-token",
    "team_id": "T061EG9R6",
    "api_app_id": "A0PNCHHK2",
    "event": {
        "type": "message",
        "channel": "C024BE91L",
        "user": "U2147483697",
        "text": "Live long and prospect.",
        "ts": "1355517523.000005",
        "event_ts": "1355517523.000005",
        "channel_type": "channel"
    },
    "type": "event_callback",
    "authed_teams": [
        "T061EG9R6"
    ],
    "event_id": "Ev0PV52K21",
    "event_time": 1355517523
}
-}


slackOptions :: Options
slackOptions = defaultOptions{ constructorTagModifier, sumEncoding }
  where
    constructorTagModifier = camelTo2 '_'
    sumEncoding = TaggedObject{ tagFieldName = "type", contentsFieldName = "_" }


data Payload
  = UrlVerification
    { token :: String
    , challenge :: String
    }
  | EventCallback
    { event :: Event
    }
  deriving (Generic)

instance FromJSON Payload where
  parseJSON = genericParseJSON slackOptions


data Event
  = Message
    { user :: Maybe String
    , text :: String
    }
  deriving (Generic)

instance FromJSON Event where
  parseJSON = genericParseJSON slackOptions


type SlackApi = "event" :> ReqBody '[JSON] Payload :> Post '[PlainText] String
