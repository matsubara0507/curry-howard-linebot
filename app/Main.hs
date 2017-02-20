{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    , getChannelSecret
    , getChannelToken
    , app
    , handler
    , handleEvent
    , handleMessageEvent
    , api
    , echo
    ) where

import CurryHowardCorrespondence (getCorrespondence)

import Control.Monad (forM_)
import Data.Maybe(fromJust)
import qualified Data.Text as T (Text, pack, unpack)
import Line.Messaging.API ( APIIO, APIError, ChannelSecret, ChannelAccessToken
                          , Message(..), runAPI, reply)
import Line.Messaging.Webhook ( Event(..), EventMessage(..), ReplyToken(..)
                              , ReplyableEvent(..), webhookApp
                              , defaultOnFailure, getMessage, getReplyToken)
import Line.Messaging.Types (Text(..))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  port <- maybe 8080 read <$> lookupEnv "PORT" :: IO Int
  run port app

getChannelSecret :: IO ChannelSecret
getChannelSecret = T.pack . fromJust <$> lookupEnv "CHANNEL_SECRET"

getChannelToken :: IO ChannelAccessToken
getChannelToken = T.pack . fromJust <$> lookupEnv "CHANNEL_TOKEN"

-- | A WAI application to handle webhook requests.
app :: Application
app req f = do
  channelSecret <- getChannelSecret
  webhookApp channelSecret handler defaultOnFailure req f

handler :: [Event] -> IO ()
handler events = forM_ events handleEvent

handleEvent :: Event -> IO ()
handleEvent (MessageEvent event) = handleMessageEvent event
handleEvent _ = return ()

handleMessageEvent :: ReplyableEvent EventMessage -> IO ()
handleMessageEvent event = do
  case getMessage event of
    TextEM _ (Text text) -> echo (getReplyToken event) (getCorrespondence text)
    _ -> echo (getReplyToken event) "undefined message"

api :: APIIO a -> IO (Either APIError a)
api = runAPI getChannelToken

echo :: ReplyToken -> T.Text -> IO ()
echo replyToken content = do
  api $ reply replyToken [ Message . Text $ content ]
  return ()
