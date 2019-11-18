module Web.Server
  ( app
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, isJust)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant
import Servant.Client

import Web.Api
import Web.Api.Slack
import Web.Api.WebHook


_ping :: Server PingApi
_ping = pure $ "Pong!"

_user :: Server UserApi
_user = greetSimple :<|> greetFull
  where
    greetSimple name = pure $ "Hello, " <> name
    greetFull User{ name, surname } = greetSimple (name <> " " <> surname)

_slack :: Server SlackApi
_slack (UrlVerification { challenge }) = do
  liftIO $ putStrLn "Verifying URL!"
  pure challenge
_slack (EventCallback (Message { user, text })) = do
  let user' = fromMaybe "bot" user
  liftIO $ putStrLn $ user' <> ": " <> text
  when (isJust user) $
    liftIO $ sendMessage $ text <> " <--"
  pure ""


_server :: Server (Api :<|> SlackApi)
_server = (_ping :<|> _user) :<|> _slack

app :: Application
app = serve (Proxy @(Api :<|> SlackApi)) _server





webHookPath :: String
webHookPath = "services/<secret redacted>"

postMessage :: WebHookRequest -> ClientM NoContent
postMessage = client (Proxy @WebHookApi)

sendMessage :: String -> IO ()
sendMessage txt = do
  manager' <- newManager tlsManagerSettings
  let baseUrl = BaseUrl Https "hooks.slack.com" 443 webHookPath
  let clientEnv = mkClientEnv manager' baseUrl
  res <- runClientM (postMessage $ WebHookRequest txt) clientEnv
  case res of
    Right NoContent -> pure ()
    Left e -> print e
