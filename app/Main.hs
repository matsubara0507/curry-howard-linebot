{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.Maybe(fromJust)
import qualified Data.Text as T (Text, pack, unpack)
import Line.Messaging.API
import Line.Messaging.Webhook
import Line.Messaging.Types (Text(..), Location(..))
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  port <- maybe 8080 read <$> lookupEnv "PORT" :: IO Int
  run port app

-- | An IO action to get channel secret. The channel secret is issued for each
-- LINE bot, and can be found in the LINE developer page.
getChannelSecret :: IO ChannelSecret
getChannelSecret = T.pack . fromJust <$> lookupEnv "CHANNEL_SECRET"

getChannelToken :: IO ChannelAccessToken
getChannelToken = T.pack . fromJust <$> lookupEnv "CHANNEL_TOKEN"

-- | A WAI application to handle webhook requests.
--
-- 'webhookApp' is used with a channel secret, event handler and error handler.
app :: Application
app req f = do
  channelSecret <- getChannelSecret
  webhookApp channelSecret handler defaultOnFailure req f

-- | An event handler.
--
-- A webhook request can contain several events at once, so the first argument
-- is a list of 'Event's. The function just call the event handler for each
-- event.
handler :: [Event] -> IO ()
handler events = forM_ events handleEvent

-- | A function to handle each event.
--
-- The type of events can be decided with pattern matching. About possible
-- types, please refer to the doc of Line.Messaging.Webhook.Types module.
--
-- Basically, content of each event has a type of 'EventTuple', which may or
-- may not contain a reply token and internal data.
handleEvent :: Event -> IO ()
handleEvent (JoinEvent event) = printSource event
handleEvent (LeaveEvent event) = printSource event
handleEvent (MessageEvent event) = handleMessageEvent event
handleEvent _ = return ()

-- | An example function to print event source of event.
--
-- Event source exists for all the event tuple types, so the reply token and
-- data types can be polymorphic.
printSource :: EventTuple r a -> IO ()
printSource event = do
  putStrLn . concatMap T.unpack $ case getSource event of
    User identifier -> [ "A user", identifier ]
    Group identifier -> [ "A group", identifier ]
    Room identifier -> [ "A room", identifier ]

-- | An example function to print text and location details of a message
--
-- A message event is always replyable and has message data in it.
handleMessageEvent :: ReplyableEvent EventMessage -> IO ()
handleMessageEvent event = do
  case getMessage event of
    TextEM _ (Text text) -> echo (getReplyToken event) text
    _ -> echo (getReplyToken event) "undefined message"

-- | An unlifter for the 'APIIO' type.
--
-- It uses the 'runAPI' function with a channel access token provided.
api :: APIIO a -> IO (Either APIError a)
api = runAPI getChannelToken

-- | An echoing action
--
-- It just send a reply with a text. By 'api', the result is
-- 'Either APIError ()', but the error is not handled for simplicity.
echo :: ReplyToken -> T.Text -> IO ()
echo replyToken content = do
  api $ reply replyToken [ Message . Text $ content ]
  return ()
